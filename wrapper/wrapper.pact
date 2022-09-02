;; The wrapper contract provides mainly the following functionalities:
;; * Boosted KDX rewards for liquidity provider fees
;; * Adding liquidity with a single side of the pair
;; * Utility functions for the frontend to query LP stats (fees collected and etc)
;;
;; It works by owning all the liquidity in behalf of users, and keeping track of
;; each user's balance by itself. The functions `add-liquidity` and `remove-liquidity`
;; are the main entrypoints into the contract.
;;
;; The KDX boosted rewards should only apply to the fees accrued by the LPs, but
;; the liquidity tokens include both the initial liquidity deposit as well as the
;; fees due to how Uniswap-style fees accrue (the fees are added to the pair
;; reserves, and the user has to withdraw liquidity to claim their fees).
;;
;; In order to differentiate between the entry amounts and the fees accrued, the
;; wrapper keeps track of the initial amount added to the liquidity pool, which
;; can be used to calculate the fees collected for a given point in time.
;;
;; This is done by calculating what IL the user would suffer without collecting
;; any fees, via the geometric mean of the entry amounts, adjusted to the current
;; reserve prices, for example, if entry reserves are x and y and the user withdraws
;; x' and y' without collecting any fees, we have that:
;;   x' = sqrt(x * y * price-of-y-in-x)
;;   y' = sqrt(x * y / price-of-y-in-x) = sqrt(x * y * price-of-x-in-y)
;; These numbers can then be used to subtract the IL-adjusted amount from the actual
;; token amounts stored in the pool to find out how many fees were collected by the user.
;; Calculating the fees this way leads to the fees in both tokens to be "equal in price",
;; that is, fees-x = fees-y * price-of-x-in-y
;;
;; This may lead to a small amount of confusion since the way fees accrue is not two-sided
;; like this. A swap only pays the fees in a single token (the input token), but this
;; should not really matter for correctness from the liquidity provider's perspective.
;; This is necessary to keep the ratio of tokens withdrawn consistent with the reserve ratio
;; of the pair.
;;
;; To prevent frontrunning and user manipulation of when the actual swaps for KDX will
;; happen when claiming boosted KDX rewards, these steps are run by a privileged keyset
;; with the OPS capability. This is necessary to get the time-weighted average price (TWAP)
;; of KDX to use for calculating the final amount the user receives (used to mitigate the
;; effect of KDX price volatility).
;;
;; When the multiplier for a given pair changes, to keep track of the previously accrued
;; fees for each user (so they can withdraw their boosted amounts later), we keep track of
;; a few separate values: the last multiplier they had, and the amount of liquidity tokens
;; that correspond to the fees accrued for the last multiplier. To update an existing stored
;; multiplier, we use a weighted average based on the amount of liquidity tokens collected
;; for the fees, so a period where the user collected more fees will be the dominant value
;; of the multiplier used for these "settled" fees.

;;(enforce-pact-version "4.3")

(namespace (read-msg 'ns))

(module wrapper GOVERNANCE
  (defschema liquidity-position
      "Tracks liquidity positions of users of the wrapper contract."
    key:string                          ;; key for this row, used for convenience
    pair:string                         ;; pair-key for this position, returned by exchange.get-pair-key
    account:string                      ;; user account that controls the liquidity tokens (`to` parameter from `add-liquidity`)
    guard:guard                         ;; guard for controlling the liquidity tokens
    liquidity-tokens:decimal            ;; amount of liquidity tokens the user can remove with `remove-liquidity`
    tokenA-pooled:decimal               ;; amount of tokenAs initially added (or later IL-adjusted)
    tokenB-pooled:decimal               ;; amount of tokenBs initially added (or later IL-adjusted)

    updated-at:time                     ;; last time the settled fee values were updated
    settled-multiplier:decimal          ;; the accumulated multiplier for the settled fees
    settled-liquidity-for-fees:decimal) ;; the amount of liquidity set aside for settled fees with the settled-multiplier
  (deftable liquidity-positions:{liquidity-position})

  (defschema liquidity-account
      "Tracks liquidity positions owned by the wrapper contract in the underlying pools."
    pair:string                         ;; pair-key for this account, returned by exchange.get-pair-key
    account:string                      ;; account created and managed by the wrapper to hold liquidity tokens
    guard:guard                         ;; module guard used to protect the held liquidity tokens
    tokenA:module{fungible-v2}          ;; module references to the two tokens of the pair
    tokenB:module{fungible-v2}
    liquidity-tokens:decimal)           ;; total amount of liquidity tokens held by the wrapper for the pair
  (deftable liquidity-accounts:{liquidity-account})

  (defschema trading-pair
      "Tracks information about supported wrapped trading pairs."
    pair:string                             ;; pair-key for this pair
    account:string                          ;; account created and managed by the wrapper to hold both tokenA/tokenB temporarily
    guard:guard                             ;; module guard used to protect the account
    fee-multiplier:decimal                  ;; the currently active KDX boosted fee multiplier for this pair
    last-fee-multiplier:decimal             ;; the last active fee multiplier for this pair
    updated-at:time                         ;; the time when the multiplier was last updated
    tokenA:module{fungible-v2}              ;; module reference to the pair token
    tokenA-base-path:[module{fungible-v2}]  ;; swap path for tokenA into KDX, e.g. [token-abc coin kaddex.kdx]
    tokenB:module{fungible-v2}
    tokenB-base-path:[module{fungible-v2}])
  (deftable trading-pairs:{trading-pair})

  (defschema reward-claim-request
      "Tracks information for pending boosted KDX reward claims."
    request-id:string  ;; key for this row, for convenience
    account:string     ;; user account claiming rewards
    to:string          ;; user account to receive the rewards
    to-guard:guard     ;; guard to receive the rewards

    liquidity-position-key:string ;; key into the liquidity-positions-table
    pair-key:string               ;; key into trading-pairs table
    tokenA-fees:decimal           ;; amount of tokenA collected as fees (not counting settled fees)
    tokenB-fees:decimal           ;; amount of tokenB collected as fees (not counting settled fees)

    tokenA-observations:[object{exchange.observation}] ;; list of TWAP observations for the tokenA swap path
    tokenB-observations:[object{exchange.observation}] ;; list of TWAP observations for the tokenB swap path
    total-kdx-swapped:decimal                          ;; total amount of KDX received from selling the fees

    fee-multiplier:decimal ;; the active multiplier the user is gonna get (not counting settled fees)

    settled-tokenA-fees:decimal ;; amount of tokenA collected as settled fees only
    settled-tokenB-fees:decimal ;; amount of tokenB collected as settled fees only
    settled-multiplier:decimal  ;; the multiplier for the settled fee amounts

    start-time:time ;; when the user initiated the request
    end-time:time   ;; when the request is available to be claimed
    status:string)  ;; current status of the request, see below for possible values
  (deftable reward-claim-requests:{reward-claim-request})

  ;; status is a simple value that progresses as follows:
  ;;     PENDING-REMOVE -> PENDING-SWAP-A -> PENDING-SWAP-B -> PENDING-TWAP -> PENDING-CLAIM -> CLAIMED
  ;; PENDING-REMOVE: the initial state when the user creates a request, measures the initial point of the TWAP, still needs to remove liquidity for the old settled-fees of the user (if applicable)
  ;; PENDING-SWAP-A/B: all tokenA/tokenB fees are held by the wrapper pair account and need to be swapped for KDX
  ;; PENDING-TWAP: the fees have been swapped for KDX and are in the WRAPPER_KDX_BANK, waiting for the time to pass to measure the TWAP a final time
  ;; PENDING-CLAIM: the TWAP measurement has been made and the end-time has been reached, the user can claim their rewards
  ;; CLAIMED: user has claimed rewards

  (defschema pending-request-schema
      requests:[string])
  (deftable pending-requests:{pending-request-schema})
  ;; if the key is account name, then requests is a list of pending requests for that account

  ;; this is a simple global lock that can be toggled by the operators to pause the contract if necessary
  (defschema contract-lock-status
      lock:bool)
  (deftable contract-lock:{contract-lock-status})
  (defconst CONTRACT_LOCK_KEY 'lock)
  (defun enforce-contract-unlocked ()
    "Asserts that the contract is not in a paused state."
    (with-read contract-lock CONTRACT_LOCK_KEY { 'lock := lock }
      (enforce (not lock) "Contract is paused")))
  (defun set-contract-lock
    ( lock:bool )
    (with-capability (OPS)
      (write contract-lock CONTRACT_LOCK_KEY {'lock: lock })))

  (defcap RESERVE_ACCESS
    ( pair:string )
    true)
  (defcap LIQUIDITY_ACCESS
    ( token:string )
    true)
  (defcap BANK_ACCESS
    ( token:string )
    true)
  (defcap MINTING
    ( token:string )
    true)
  (defcap BURNING
    ( token:string )
    true)

  (defun format-token:string (token:module{fungible-v2})
    (format "{}" [token]))

  (defun enforce-bank-access (token:string)
    (require-capability (BANK_ACCESS token)))
  (defun enforce-reserve-access (pair:string)
    (require-capability (RESERVE_ACCESS pair)))
  (defun enforce-minting (token:string)
    (require-capability (MINTING token)))
  (defun enforce-burning (token:string)
    (require-capability (BURNING token)))
  (defun enforce-liquidity-access (pair:string)
    (require-capability (LIQUIDITY_ACCESS pair)))

  (defun create-reserve-guard (pair-key:string)
    (create-user-guard (enforce-reserve-access pair-key)))
  (defun create-mint-guard (token:module{fungible-v2})
    (create-user-guard (enforce-minting (format-token token))))
  (defun create-burn-guard (token:module{fungible-v2})
    (create-user-guard (enforce-burning (format-token token))))
  (defun create-liquidity-guard (pair:string)
    (create-user-guard (enforce-liquidity-access pair)))
  (defun create-bank-guard ()
    (create-user-guard (enforce-bank-access (format-token (get-base-token)))))

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'kaddex-wrapper-admin)))

  (defcap OPS ()
    (enforce-guard (keyset-ref-guard 'kaddex-wrapper-ops)))

  (defcap LIQUIDITY_POS_GUARD
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      account:string
    )
    "Capability for enforcing the user's liquidity-position guard"
    (enforce-guard (at 'guard (get-liquidity-position token0 token1 account)))
  )

  (defcap FEE_BUYBACK
    ( token:module{fungible-v2}
      token-accrued:decimal
    )
    "Private capability for processing KDX buybacks."
    true
  )

  (defcap SWAPPING_FEES ()
    "Private capability for swapping KDX."
    true
  )

  (defcap FEE_REWARD
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      token0-accrued:decimal
      token1-accrued:decimal
      base-token-paid:decimal
      reward-multiplier:decimal
      bonus-reward:decimal
      to:string
    )
    "Event for reporting fee payouts."
    @event
    true
  )

  ;; minimum amount of tokens to be eligible for the KDX booster (NOTE: about $0.40 worth of BTC currently)
  (defconst MINIMUM_FEE_FOR_BOOSTER:decimal 0.00001)

  ;; account name for the wrapper KDX holding account, holds the KDX from swapping user fees
  (defconst WRAPPER_KDX_BANK 'kaddex-kdx-wrapper-bank)

  ;; account name for holding pre-minted KDX for booster distribution. needs to be regularly monitored and possibly refilled
  ;; this is used instead of giving mint permissions to the wrapper to minimize attack surface
  (defconst WRAPPER_KDX_MINT_BANK 'kaddex-kdx-wrapper-mint-bank)

  ;; how long users need to wait before they can claim their KDX boosted rewards
  (defconst BOOSTED_REWARD_VESTING_TIME (days 7))

  ;; utility function to get a module reference to KDX
  (defun get-base-token:module{fungible-v2} () kaddex.kdx)

  (defun is-base-path:bool
    ( token:module{fungible-v2}
      path:[module{fungible-v2}]
    )
    "Checks if the given path is valid for swapping token into base-token"
    (let ((base-token (get-base-token)))
      (if (tokens-equal token base-token)
          (= (map (format-token) path) [(format-token base-token)]) ;; base token does not need swapping
          (let* ( (path-length (length path))
                  (last-index (- path-length 1))
                  (path-component-exists
                  (lambda (i)
                    (let ((path0 (at (- i 1) path))
                          (path1 (at i path)))
                      (exchange.pair-exists path0 path1))))
                  (path-is-tracked (exchange.is-path-tracked path))
                )
            (and (>= path-length 2) ;; the path needs 2 or more components
                 (and path-is-tracked ;; the path needs to be tracked by the TWAP oracle
                      (and (and (tokens-equal (at 0 path) token) ;; path needs to start with token
                                (tokens-equal (at last-index path) base-token)) ;; and end with base-token
                           (fold (and) true (map (path-component-exists) (enumerate 1 last-index)))))) ;; every pair in the path needs to exist
          )
      )
    )
  )

  (defun tokens-equal:bool
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Token equality defined by comparing the fully qualified contract names."
    ;; Module references are equal if and only if they implement exactly the same interfaces.
    ;; Updates to tokens (such as implementing the new fungible-xchain-v1 interface) can
    ;; break this, so we only check if the fully qualified name is the same.
    (= (format "{}" [tokenA])
       (format "{}" [tokenB]))
  )

  (defun get-pair-account:string
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (at 'account (read trading-pairs (exchange.get-pair-key tokenA tokenB) ['account]))
  )

  (defun get-pair-multiplier:decimal
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (at 'fee-multiplier (read trading-pairs (exchange.get-pair-key tokenA tokenB) ['fee-multiplier]))
  )

  (defun get-liquidity-position-key
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      account:string
    )
    (format "{}:{}" [(exchange.get-pair-key tokenA tokenB) account])
  )

  (defun get-liquidity-account:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (read liquidity-accounts (exchange.get-pair-key tokenA tokenB))
  )

  (defun get-liquidity-position:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      account:string
    )
    (read liquidity-positions (get-liquidity-position-key tokenA tokenB account))
  )

  (defun pair-registered:bool
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (with-default-read trading-pairs (exchange.get-pair-key tokenA tokenB)
      { 'pair: "" }
      { 'pair := read-pair-key }
      (!= 0 (length read-pair-key))
    )
  )

  (defun register-pair-only
    ( tokenAA:module{fungible-v2}
      tokenBB:module{fungible-v2}
      tokenAA-base-path:[module{fungible-v2}]
      tokenBB-base-path:[module{fungible-v2}]
      hint:string
    )
    "Registers a pair with the wrapper. This function only creates accounts and sets up state, but does not add liquidity."
    (enforce-contract-unlocked)
    (with-capability (OPS)
      (let*
        ( (pair-key (exchange.get-pair-key tokenAA tokenBB))
          (canon (exchange.is-canonical tokenAA tokenBB))
          (tokenA:module{fungible-v2} (if canon tokenAA tokenBB))
          (tokenA-base-path:[module{fungible-v2}] (if canon tokenAA-base-path tokenBB-base-path))
          (tokenB:module{fungible-v2} (if canon tokenBB tokenAA))
          (tokenB-base-path:[module{fungible-v2}] (if canon tokenBB-base-path tokenAA-base-path))
          (both-tokens-have-base-paths
            (and (is-base-path tokenA tokenA-base-path)
                (is-base-path tokenB tokenB-base-path)))
        )
        (enforce (exchange.is-canonical tokenA tokenB) "Sanity check -- should never fail")
        (enforce both-tokens-have-base-paths "Base paths for both tokens must be provided and oracles must track them")
        (let*
          ( (liquidity-account-name (hash (+ hint (+ pair-key (format "{}_liq" [(at 'block-time (chain-data))])))))
            (pair-account-name (hash (+ hint (+ pair-key (format "{}_pair" [(at 'block-time (chain-data))])))))
            (pair-guard (create-reserve-guard pair-key))
            (liquidity-guard (create-liquidity-guard pair-key))
          )
          (tokenA::create-account pair-account-name pair-guard)
          (tokenB::create-account pair-account-name pair-guard)
          (tokens.create-account pair-key liquidity-account-name liquidity-guard)
          (insert trading-pairs pair-key
            { 'pair: pair-key
            , 'account: pair-account-name
            , 'guard: pair-guard
            , 'fee-multiplier: 1.0
            , 'updated-at: (at 'block-time (chain-data))
            , 'last-fee-multiplier: 0.0
            , 'tokenA: tokenA
            , 'tokenA-base-path: tokenA-base-path
            , 'tokenB: tokenB
            , 'tokenB-base-path: tokenB-base-path
            })
          (insert liquidity-accounts pair-key
            { 'pair: pair-key
            , 'account: liquidity-account-name
            , 'tokenA: tokenA
            , 'tokenB: tokenB
            , 'liquidity-tokens: 0.0
            , 'guard: liquidity-guard
            })))))

  (defun register-pair
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      tokenA-base-path:[module{fungible-v2}]
      tokenB-base-path:[module{fungible-v2}]
      amountA-desired:decimal
      amountB-desired:decimal
      amountA-min:decimal
      amountB-min:decimal
      sender:string
      hint:string
    )
    "Registers a pair with the wrapper. This function also adds liquidity to the pair."
    (enforce-contract-unlocked)
    (with-capability (OPS)
      (enforce (exchange.is-canonical tokenA tokenB) "please make my life easier")
      (register-pair-only tokenA tokenB tokenA-base-path tokenB-base-path hint)
      (let
        ( (liquidity-account-name (at 'account (get-liquidity-account tokenA tokenB)))
          (pair-key (exchange.get-pair-key tokenA tokenB))
        )
        (with-capability (LIQUIDITY_ACCESS pair-key)
          (exchange.add-liquidity tokenA tokenB
                                  amountA-desired amountB-desired
                                  amountA-min amountB-min
                                  sender liquidity-account-name (create-liquidity-guard pair-key)))
      )
    )
  )

  (defun set-fee-multiplier-for-pair
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      new-multiplier:decimal
    )
    "Sets a new multiplier for the pair."
    ;; when calling this function, it is necessary to also get a list of all accounts in the pair
    ;; to call `update-single-position-for-new-multiplier`, otherwise the user balances will not
    ;; be updated and they will only be affected by the new multiplier and not the old one
    (enforce-contract-unlocked)
    (with-capability (OPS)
      (enforce (>= new-multiplier 1.0) "Fee multiplier must be positive and greater than 1.")
      (let ((pair-key (exchange.get-pair-key tokenA tokenB)))
        (with-read trading-pairs pair-key
          { 'fee-multiplier := current-multiplier }
          (update trading-pairs pair-key
            { 'fee-multiplier: new-multiplier
            , 'last-fee-multiplier: current-multiplier
            , 'updated-at: (at 'block-time (chain-data))
            }
          )
          (format "Fee multiplier on {} changed from {}x to {}x." [ pair-key current-multiplier new-multiplier ])))))

  (defun get-total-supply-considering-fees:decimal
    ( pair:object
      pair-key:string
    )
    "Returns the value of `tokens.total-supply` considering the effect `exchange.mint-fee` would have in dilluting the supply. This ensures that fee calculations are consistent with what would happen when calling `add-liquidity` or `remove-liquidity`."
    (let*
      ( (token0-reserves (at 'reserve (at 'leg0 pair)))
        (token1-reserves (at 'reserve (at 'leg1 pair)))
        ;; the logic below is mostly copied from mint-fee directly
        (current-k (sqrt (* token0-reserves token1-reserves)))
        (last-k (at 'last-k pair))
        (unminted-fee-liquidity
        (if (and (> exchange.FEE_RESERVED 0.0) (> (at 'last-k pair) 0.0))
            (let*
                ( (fee-denominator (- (/ exchange.FEE exchange.FEE_RESERVED) 1.0))
                  (numerator (* (tokens.total-supply pair-key) (- current-k last-k)))
                  (denominator (+ (* fee-denominator current-k) last-k))
                  (fee-liquidity (tokens.truncate pair-key (/ numerator denominator)))
                  )
              (if (> fee-liquidity 0.0) fee-liquidity 0.0)
              )
            0.0))
        (liquidity-supply (+ (tokens.total-supply pair-key) unminted-fee-liquidity)))
      liquidity-supply))

  (defun update-single-position-for-new-multiplier
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      account:string
    )
    "Updates the settled amounts for the liquidity-position using the last multiplier."
    (enforce-contract-unlocked)
    (enforce (exchange.is-canonical tokenA tokenB) "The tokens are not sorted")
    (with-capability (OPS)
      (with-read trading-pairs (exchange.get-pair-key tokenA tokenB)
        { 'updated-at := pair-updated-at
        , 'last-fee-multiplier := last-multiplier
        }
        (with-read liquidity-positions (get-liquidity-position-key tokenA tokenB account)
          { 'updated-at := pos-updated-at
          , 'liquidity-tokens := user-liquidity
          , 'tokenA-pooled := tokenA-pooled
          , 'tokenB-pooled := tokenB-pooled
          , 'settled-multiplier := settled-multiplier
          , 'settled-liquidity-for-fees := settled-liquidity
          }
          (if (< pos-updated-at pair-updated-at) ;; do nothing if position is already updated
              (let* ;; if we need to update the position, we need to calculate the amount of fees accrued
                ( (pair-key (exchange.get-pair-key tokenA tokenB))
                  (pair (exchange.get-pair tokenA tokenB))
                  (user-tokens (get-token-amounts-for-liquidity tokenA tokenB user-liquidity))
                  (tokenA-reserves (exchange.reserve-for pair tokenA))
                  (tokenB-reserves (exchange.reserve-for pair tokenB))
                  ;; IL-adjust the token amounts
                  (tokenA-total (at 'amountA user-tokens))
                  (tokenB-total (at 'amountB user-tokens))
                  (tokenA-adjusted (min tokenA-total (exchange.truncate tokenA (sqrt (* (* tokenA-pooled tokenB-pooled) (/ tokenA-reserves tokenB-reserves))))))
                  (tokenB-adjusted (min tokenB-total (exchange.truncate tokenB (sqrt (* (* tokenA-pooled tokenB-pooled) (/ tokenB-reserves tokenA-reserves))))))
                  ;; calculate the fees
                  (tokenA-fees (max 0.0 (exchange.truncate tokenA (- tokenA-total tokenA-adjusted))))
                  (tokenB-fees (max 0.0 (exchange.truncate tokenB (- tokenB-total tokenB-adjusted))))
                  ;; get the current total supply
                  (liquidity-supply (get-total-supply-considering-fees pair pair-key))
                  ;; calculate how many liquidity tokens correspond to the fees
                  (tokenA-liquidity (tokens.truncate pair-key (* (/ tokenA-fees tokenA-reserves) liquidity-supply)))
                  (tokenB-liquidity (tokens.truncate pair-key (* (/ tokenB-fees tokenB-reserves) liquidity-supply)))
                  (fees-liquidity (max tokenA-liquidity tokenB-liquidity))
                  ;; add up the new liquidity with the old settled amounts
                  (total-fee-liquidity (+ settled-liquidity fees-liquidity))
                  ;; update the multiplier using a weighted average using the liquidity as weights
                  (new-multiplier (try 0.0 (+ (* (/ settled-liquidity total-fee-liquidity) settled-multiplier) (* (/ fees-liquidity total-fee-liquidity) last-multiplier))))
                )
                (if (and (> tokenA-fees MINIMUM_FEE_FOR_BOOSTER) (> tokenB-fees MINIMUM_FEE_FOR_BOOSTER))
                    (let ((dummy 'dummy))
                      ;; check that tokenA-liquidity and tokenB-liquidity agree up to a tiny rounding error
                      (enforce (<= (abs (- tokenA-liquidity tokenB-liquidity)) 0.000000000005) "sanity check -- liquidity amounts mismatch between A and B")
                      (enforce (= 12 (tokens.precision pair-key)) "sanity check -- token precision mismatch")
                      (enforce (>= user-liquidity fees-liquidity) "sanity check -- user liquidity is greater than fees")
                      ;; update the liquidity position with the new values
                      (update liquidity-positions (get-liquidity-position-key tokenA tokenB account)
                        { 'settled-multiplier: new-multiplier
                        , 'settled-liquidity-for-fees: total-fee-liquidity
                        , 'updated-at: (at 'block-time (chain-data))
                        , 'liquidity-tokens: (- user-liquidity fees-liquidity)
                        })
                      (format "updated: {} / {} / {} / {} / {} / {}" [tokenA-fees tokenB-fees new-multiplier total-fee-liquidity fees-liquidity user-liquidity])
                    )
                    (format "not updated: {} / {} / {} / {} / {} / {}" [tokenA-fees tokenB-fees new-multiplier total-fee-liquidity fees-liquidity user-liquidity])
                  )
              )
              (format "not updated: {} vs {}" [pos-updated-at pair-updated-at]))
        )
      )
    )
  )

  (defun update-positions-for-new-multiplier
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      accounts:[string]
    )
    "Simple (inefficient) batching for `update-single-position-for-new-multiplier`."
    (map (lambda (x) (update-single-position-for-new-multiplier tokenA tokenB x)) accounts)
  )

  ;; TODO: should be in some util module
  (defun max:decimal (a:decimal b:decimal)
    (if (> a b) a b))
  (defun min:decimal (a:decimal b:decimal)
    (if (< a b) a b))
  (defun min-int:integer (a:integer b:integer)
    (if (< a b) a b))
  (defun remove-elem-from-list (elem lst)
    (filter (lambda (x) (!= x elem)) lst))

  (defun get-oracle-time-cumulative-price:[object{exchange.observation}]
    ( path:[module{fungible-v2}] )
    (enforce (> (length path) 0) "wrapper: non-empty path")
    (if (> (length path) 1)
        [(exchange.get-oracle-time-cumulative-price path)]
        [] ;; return a dummy value for 1-length paths: this value is not used in this case, the price is always 1.0
    )
  )

  (defun remove-liquidity:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      requested-liquidity:decimal
      amountA-min:decimal
      amountB-min:decimal
      sender:string
      to:string
      to-guard:guard
      wants-kdx-rewards:bool
    )
    "Wrapper around `exchange.remove-liquidity` for wrapper-managed positions. If `wants-kdx-rewards` is true, the user receives their (IL-adjusted) initial investment now, and creates a reward claim request for the KDX boosted rewards. Otherwise, the user receives their full amount, as well as any previously settled fees due to multiplier changes."
    (enforce-contract-unlocked)
    (enforce (and (and (> requested-liquidity 0.0) (>= amountA-min 0.0)) (>= amountB-min 0.0)) "remove-liquidity: Values must be positive")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= to "") "Invalid target")
    (enforce (exchange.is-canonical tokenA tokenB) "The tokens are not sorted")
    (let*
      ( (p (exchange.get-pair tokenA tokenB))
        (pair-key (exchange.get-pair-key tokenA tokenB))
        (token0:module{fungible-v2} (at 'token (at 'leg0 p)))
        (token1:module{fungible-v2} (at 'token (at 'leg1 p)))
        (liquidity-account (get-liquidity-account tokenA tokenB))
        (liquidity-account-name (at 'account liquidity-account))
        (entry-total-liquidity (at 'liquidity-tokens liquidity-account))
        (actual-total-liquidity (tokens.get-balance pair-key liquidity-account-name))
        (liquidity-position-key (get-liquidity-position-key tokenA tokenB sender))
        (liquidity-position (get-liquidity-position tokenA tokenB sender))
        (liquidity-position-share (at 'liquidity-tokens liquidity-position))
        (withdrawal-fraction (/ requested-liquidity liquidity-position-share))
        (pair-account (read trading-pairs pair-key))
        (pair-account-name (at 'account pair-account))
        (pair-account-guard (at 'guard pair-account))
      )
      ;; verify the liquidity position guard and invariants
      (install-capability (LIQUIDITY_POS_GUARD tokenA tokenB sender))
      (enforce (tokens-equal tokenA token0) "sanity check: token0 is tokenA")
      (enforce (tokens-equal tokenB token1) "sanity check: token1 is tokenB")
      (enforce (<= requested-liquidity entry-total-liquidity)
        (format "remove-liquidity: Insufficient liquidity position ({} > {})" [requested-liquidity, entry-total-liquidity]))
      (enforce (<= requested-liquidity actual-total-liquidity) "sanity check: actual-total-liquidity covers the amount")
      (enforce (> requested-liquidity 0.0) "remove-liquidity: liquidity must be positive and non-zero")
      (enforce (<= requested-liquidity liquidity-position-share) "remove-liquidity: Insufficient liquidity position")
      (enforce (and (> withdrawal-fraction 0.0) (<= withdrawal-fraction 1.0)) "sanity check: cannot remove more liquidity than user owns")

      (let*
        ( (settled-fee-liquidity (at 'settled-liquidity-for-fees liquidity-position))
          (withdrawn-liquidity (if (not wants-kdx-rewards) (+ requested-liquidity settled-fee-liquidity) requested-liquidity))
        )
        (enforce (<= withdrawn-liquidity entry-total-liquidity) "sanity check: the settled liquidity does not overflow the owned liquidity")
        (enforce (<= withdrawn-liquidity actual-total-liquidity) "sanity check: actual-total-liquidity covers the updated amount")

        (install-capability (tokens.TRANSFER pair-key liquidity-account-name (at 'account p) withdrawn-liquidity))
        (let*
          ( (tokenA-initially-pooled (at 'tokenA-pooled liquidity-position))
            (tokenB-initially-pooled (at 'tokenB-pooled liquidity-position))
            (remove-liquidity-result
              (with-capability (LIQUIDITY_ACCESS pair-key)
                (exchange.remove-liquidity
                  tokenA tokenB withdrawn-liquidity amountA-min amountB-min liquidity-account-name pair-account-name pair-account-guard))
            )
            (tokenA-current-reserves (exchange.reserve-for p tokenA))
            (tokenB-current-reserves (exchange.reserve-for p tokenB))
            (tokenA-withdrawn (at 'amount0 remove-liquidity-result))
            (tokenB-withdrawn (at 'amount1 remove-liquidity-result))
            ;; calculate the total amount of fees withdrawn by IL-adjusting the initial amounts
            ;; here we multiply by `withdrawal-fraction` in case the user only withdrew a part of their liquidity tokens
            (tokenA-adjusted-pooled (exchange.truncate tokenA (* withdrawal-fraction (sqrt (* (* tokenA-initially-pooled tokenB-initially-pooled) (/ tokenA-current-reserves tokenB-current-reserves))))))
            (tokenB-adjusted-pooled (exchange.truncate tokenB (* withdrawal-fraction (sqrt (* (* tokenA-initially-pooled tokenB-initially-pooled) (/ tokenB-current-reserves tokenA-current-reserves))))))
            ;; calculate the fees and the users' remaining liquidity tokens
            (tokenA-fees (exchange.truncate tokenA (- tokenA-withdrawn tokenA-adjusted-pooled)))
            (tokenB-fees (exchange.truncate tokenB (- tokenB-withdrawn tokenB-adjusted-pooled)))
            (will-create-reward-claim (and wants-kdx-rewards (or (> settled-fee-liquidity 0.0) (and (> tokenA-fees MINIMUM_FEE_FOR_BOOSTER) (> tokenB-fees MINIMUM_FEE_FOR_BOOSTER)))))
            (now (at 'block-time (chain-data)))
            (request-id (hash (format "{}-{}-{}-{}-{}" [now sender pair-key withdrawn-liquidity withdrawal-fraction])))
            (remaining-total-liquidity (- entry-total-liquidity withdrawn-liquidity))
          )
          ;; check if we will give out the fees in KDX or directly
          (if will-create-reward-claim
              (let ((dummy 'dumb)) ;; give them the adjusted entry amounts and handle fee payout via reward-claim-requests
                ;; check some sanity check invariants
                (enforce (= (+ tokenA-adjusted-pooled tokenA-fees) tokenA-withdrawn) "tokenA amounts must match")
                (enforce (= (+ tokenB-adjusted-pooled tokenB-fees) tokenB-withdrawn) "tokenB amounts must match")
                (enforce (>= tokenA-fees 0.0) "fees must be non-negative")
                (enforce (>= tokenB-fees 0.0) "fees must be non-negative")

                ;; transfer the base adjusted amounts to the user
                (install-capability (token0::TRANSFER pair-account-name to tokenA-adjusted-pooled))
                (with-capability (RESERVE_ACCESS pair-key)
                  (token0::transfer-create pair-account-name to to-guard tokenA-adjusted-pooled))
                (install-capability (token1::TRANSFER pair-account-name to tokenB-adjusted-pooled))
                (with-capability (RESERVE_ACCESS pair-key)
                  (token1::transfer-create pair-account-name to to-guard tokenB-adjusted-pooled))

                (let*
                  ( (fee-multiplier (at 'fee-multiplier pair-account))
                    (tokenA-obs (get-oracle-time-cumulative-price (at 'tokenA-base-path pair-account)))
                    (tokenB-obs (get-oracle-time-cumulative-price (at 'tokenB-base-path pair-account)))
                  ) ;; get the information necessary for the reward claim request
                  ;; the initial TWAP used for the KDX booster is sampled at this time
                  (insert reward-claim-requests request-id
                    { 'request-id: request-id
                    , 'account: sender
                    , 'to: to
                    , 'to-guard: to-guard
                    , 'liquidity-position-key: liquidity-position-key
                    , 'pair-key: pair-key
                    , 'tokenA-fees: tokenA-fees
                    , 'tokenB-fees: tokenB-fees
                    , 'tokenA-observations: tokenA-obs
                    , 'tokenB-observations: tokenB-obs
                    , 'total-kdx-swapped: 0.0 ;; this is filled out later
                    , 'fee-multiplier: fee-multiplier
                    , 'settled-tokenA-fees: 0.0 ;; these settled values are filled out later if necessary
                    , 'settled-tokenB-fees: 0.0
                    , 'settled-multiplier: 0.0
                    , 'start-time: now
                    , 'end-time: (add-time now BOOSTED_REWARD_VESTING_TIME)
                    , 'status: 'PENDING-REMOVE
                    })
                  ;; add the new pending request to the list of pending requests
                  (with-default-read pending-requests sender { 'requests: [] } { 'requests := requests }
                    (write pending-requests sender { 'requests: (+ requests [request-id]) }))
                )
              )
              (let ((dummy 'dumb))
                ;; fallback case: they do not want kdx rewards, OR fees are too low, just give them the tokens withdrawn
                (install-capability (token0::TRANSFER pair-account-name to tokenA-withdrawn))
                (with-capability (RESERVE_ACCESS pair-key)
                  (token0::transfer-create pair-account-name to to-guard tokenA-withdrawn))
                (install-capability (token1::TRANSFER pair-account-name to tokenB-withdrawn))
                (with-capability (RESERVE_ACCESS pair-key)
                  (token1::transfer-create pair-account-name to to-guard tokenB-withdrawn))

                ;; update the liquidity position settled fee if the user withdrew their settled fees
                (if (not wants-kdx-rewards)
                    (update liquidity-positions liquidity-position-key
                      { 'updated-at: now
                      , 'settled-multiplier: 0.0
                      , 'settled-liquidity-for-fees: 0.0
                      })
                    true
                )
              )
          )
          ;; update the users' liquidity position
          (update liquidity-positions liquidity-position-key
            { 'liquidity-tokens: (- liquidity-position-share requested-liquidity) ;; NOTE: this table field does NOT include the settled fee liquidity
            , 'tokenA-pooled: (- (at 'tokenA-pooled liquidity-position) (* withdrawal-fraction tokenA-initially-pooled))
            , 'tokenB-pooled: (- (at 'tokenB-pooled liquidity-position) (* withdrawal-fraction tokenB-initially-pooled))
            })
          ;; update the pair's liquidity account balance
          (update liquidity-accounts pair-key { 'liquidity-tokens: remaining-total-liquidity })

          (if will-create-reward-claim
              { 'amountA: tokenA-adjusted-pooled
              , 'amountB: tokenB-adjusted-pooled
              , 'request-id: request-id
              , 'claim-time: (add-time now BOOSTED_REWARD_VESTING_TIME)
              , 'withdrawn-directly: false
              }
              { 'amountA: tokenA-withdrawn
              , 'amountB: tokenB-withdrawn
              , 'withdrawn-directly: true
              }
          )
        )
      )

    )
  )

  (defun get-token-amounts-for-liquidity:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      liquidity:decimal
    )
    "Returns the amount of tokens you would get by removing the specified amount of liquidity from the given pair."
    (let*
      ( (p (exchange.get-pair tokenA tokenB))
        (pair-account (at 'account p))
        (pair-key (exchange.get-pair-key tokenA tokenB))
        (reserveA (exchange.reserve-for p tokenA))
        (reserveB (exchange.reserve-for p tokenB))
        (total-supply (get-total-supply-considering-fees p pair-key))
        (share (/ liquidity total-supply))
        (amountA (exchange.truncate tokenA (* share reserveA)))
        (amountB (exchange.truncate tokenB (* share reserveB)))
      )
      { 'amountA: amountA, 'amountB: amountB, 'share: (floor share 10) }
    )
  )

  (defun get-user-position-stats:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      sender:string
    )
    "Returns friendly information containing IL-adjusted entry amounts, fees accrued, liquidity tokens amount and total user pool share."
    (let*
      ( (p (exchange.get-pair tokenA tokenB))
        (pair-key (exchange.get-pair-key tokenA tokenB))
        (liquidity-position (get-liquidity-position tokenA tokenB sender))
        (user-liquidity (at 'liquidity-tokens liquidity-position))
        (user-tokens (get-token-amounts-for-liquidity tokenA tokenB user-liquidity))
        (user-share (at 'share user-tokens))
        (settled-multiplier (at 'settled-multiplier liquidity-position))
        (fee-multiplier (at 'fee-multiplier (read trading-pairs pair-key ['fee-multiplier])))
        ;; we need to combine information of settled fees together with the rest
        (fee-liquidity (at 'settled-liquidity-for-fees liquidity-position))
        (fee-tokens (get-token-amounts-for-liquidity tokenA tokenB fee-liquidity))
        (settled-fee-tokenA (exchange.truncate tokenA (at 'amountA fee-tokens)))
        (settled-fee-tokenB (exchange.truncate tokenB (at 'amountB fee-tokens)))
        (fee-share (at 'share fee-tokens))
        ;; calculate the fees by IL-adjusting the amounts
        (tokenA-initially-pooled (at 'tokenA-pooled liquidity-position))
        (tokenB-initially-pooled (at 'tokenB-pooled liquidity-position))
        (tokenA-current-reserves (exchange.reserve-for p tokenA))
        (tokenB-current-reserves (exchange.reserve-for p tokenB))
        (tokenA-amount (at 'amountA user-tokens))
        (tokenB-amount (at 'amountB user-tokens))
        ;; clamp at token amounts and 0.0 since this is what we display to the user, just in case
        (tokenA-adjusted-pooled (min tokenA-amount (exchange.truncate tokenA (sqrt (* (* tokenA-initially-pooled tokenB-initially-pooled) (/ tokenA-current-reserves tokenB-current-reserves))))))
        (tokenB-adjusted-pooled (min tokenB-amount (exchange.truncate tokenB (sqrt (* (* tokenA-initially-pooled tokenB-initially-pooled) (/ tokenB-current-reserves tokenA-current-reserves))))))
        (tokenA-fees (max 0.0 (exchange.truncate tokenA (- tokenA-amount tokenA-adjusted-pooled))))
        (tokenB-fees (max 0.0 (exchange.truncate tokenB (- tokenB-amount tokenB-adjusted-pooled))))
        (total-feesA (exchange.truncate tokenA (+ tokenA-fees settled-fee-tokenA)))
        (total-feesB (exchange.truncate tokenB (+ tokenB-fees settled-fee-tokenB)))
        (totalA (exchange.truncate tokenA (+ tokenA-amount settled-fee-tokenA)))
        (totalB (exchange.truncate tokenB (+ tokenB-amount settled-fee-tokenB)))
        (total-share (+ user-share fee-share))
      )
      { 'totalA: totalA, 'totalB: totalB
      , 'feesA: total-feesA, 'feesB: total-feesB
      , 'settled-feesA: settled-fee-tokenA, 'settled-feesB: settled-fee-tokenB
      , 'unsettled-feesA: tokenA-fees, 'unsettled-feesB: tokenB-fees
      , 'initialA: tokenA-adjusted-pooled, 'initialB: tokenB-adjusted-pooled
      , 'liquidity: user-liquidity, 'user-pool-share: total-share
      , 'settled-multiplier: settled-multiplier, 'current-multiplier: fee-multiplier
      }
    )
  )

  (defun preview-remove-liquidity
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      account:string
      amount:decimal
    )
    (enforce (> amount 0.0) "amount needs to be positive")
    (with-read trading-pairs (exchange.get-pair-key tokenA tokenB)
      { 'tokenA-base-path := tokenA-base-path
      , 'tokenB-base-path := tokenB-base-path
      }
      ;; fees are settled-fees + ((withdrawal-fraction)*(total-token-amount) - (withdrawal-fraction)*(adjusted-initial-amount))
      ;; this is because settled fees are always removed completely, differently from the rest of the tokens
      (let*
        ( (user-stats (get-user-position-stats tokenA tokenB account))
          (amount-share (/ amount (at 'liquidity user-stats)))
          (amountA (exchange.truncate tokenA (* amount-share (at 'initialA user-stats))))
          (amountB (exchange.truncate tokenB (* amount-share (at 'initialB user-stats))))
          (settled-feesA (at 'settled-feesA user-stats))
          (settled-feesB (at 'settled-feesB user-stats))
          (unsettled-feesA (exchange.truncate tokenA (* amount-share (at 'unsettled-feesA user-stats))))
          (unsettled-feesB (exchange.truncate tokenB (* amount-share (at 'unsettled-feesB user-stats))))
          (feesA (exchange.truncate tokenA (+ settled-feesA unsettled-feesA)))
          (feesB (exchange.truncate tokenB (+ settled-feesB unsettled-feesB)))
          (tokenA-kdx-price (exchange.get-spot-price-for-path tokenA-base-path))
          (tokenB-kdx-price (exchange.get-spot-price-for-path tokenB-base-path))
          (settled-multiplier (at 'settled-multiplier user-stats))
          (estimated-settled-fees (+ (* (* settled-feesA tokenA-kdx-price) settled-multiplier) (* (* settled-feesB tokenB-kdx-price) settled-multiplier)))
          (current-multiplier (at 'current-multiplier user-stats))
          (estimated-unsettled-fees (+ (* (* unsettled-feesA tokenA-kdx-price) current-multiplier) (* (* unsettled-feesB tokenB-kdx-price) current-multiplier)))
          (estimated-boosted-kdx (exchange.truncate (get-base-token) (+ estimated-settled-fees estimated-unsettled-fees)))
        )
        (enforce (<= amount (at 'liquidity user-stats)) "amount needs to be less than or equal to user liquidity")
        { 'tokenA-amount-received: amountA, 'tokenB-amount-received: amountB
        , 'estimated-kdx-rewards: estimated-boosted-kdx
        , 'tokenA-fees-received: feesA, 'tokenB-fees-received: feesB
        }
      )
    )
  )

  (defun total-kdx-rewards-available:decimal
    ()
    (let*
      ( (network-rewards-minted (at 'total-minted (kdx.get-raw-supply 'network-rewards)))
        (network-rewards-total (kdx.get-purpose-max-cap 'network-rewards))
        (wrapper-mint-bank-balance (kdx.get-balance WRAPPER_KDX_MINT_BANK))
      )
      ;; we take the total network rewards, and remove whatever has been minted already
      ;; we ignore the WRAPPER_KDX_MINT_BANK balance because those are rewards available for distribution
      (exchange.truncate kdx (- network-rewards-total (- network-rewards-minted wrapper-mint-bank-balance)))
    )
  )

  (defun get-one-sided-liquidity-swap-amount:decimal
    ( A:decimal ;; reserves of tokenA
      T:decimal ;; total amount of tokens (A)
    )
    "Internal low-level function used for calculating one-sided liquidity swap amounts."
    ;; this returns an input amount of tokens to swap for B that satisfies:
    ;; (= (quote (- T x) A B) (compute-out x A B))
    ;; this value needs to be truncated to the token precision (and may not
    ;; be exact and leave some dust behind)
    (let ((fee (- 1.0 exchange.FEE)))
      (/ (+ (* (sqrt A)
               (sqrt (+ (* A (* fee fee))
                        (+ (* 2 (* A fee))
                           (+ A
                              (* 4 (* fee T)))))))
            (+ (* A (* fee -1))
               (* A -1)))
         (* 2 fee))))

  (defun get-other-side-token-amount-after-swap:decimal
    ( amountA-total:decimal
      tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      slippage:decimal ;; this value needs to be greater than or equal to 1
      ;; a value of 1.01 gives a 1% slippage
    )
    "Returns the tokenB amount to use for signing a TRANSFER capability when doing an `add-liquidity-one-sided` call."
    ;; this function is used for signing a TRANSFER capability for tokenB
    ;; for use with add-liquidity-one-sided. We transfer the swapped tokens
    ;; back to the sender and then transfer from the sender to the liquidity
    ;; pool (and thus need the TRANSFER cap for the final step)
    (let*
      ( (p (exchange.get-pair tokenA tokenB))
        (reserveA (exchange.reserve-for p tokenA))
        (amountB-in (exchange.truncate tokenB (get-one-sided-liquidity-swap-amount reserveA amountA-total)))
        (alloc { 'token-out: tokenA
               , 'token-in: tokenA
               , 'out: amountB-in
               , 'in: 0.0
               , 'idx: 0
               , 'pair: p
               , 'path: [tokenB]})
        (out-result (exchange.compute-out [alloc] tokenB))
      )
      ;; multiply by the slippage in case the price changes between the user
      ;; querying this function and calling the function below
      (exchange.truncate tokenB (* slippage (at 'out (at 0 out-result))))
    )
  )

  (defun add-liquidity-one-sided:object
    ( token-in:module{fungible-v2}
      token-other:module{fungible-v2}
      amount-in-total:decimal
      amount-in-min:decimal
      amount-other-min:decimal
      sender:string
      sender-guard:guard
      to:string
      to-guard:guard
    )
    "Adds liquidity to the tokenA/tokenB pair using only tokenA. Will automatically swap about half of the amountA-total into tokenB to provide the liquidity."
    (enforce-contract-unlocked)
    (enforce (and (and (> amount-in-total 0.0) (>= amount-in-min 0.0)) (>= amount-other-min 0.0)) "add-liquidity-one-sided: Values must be positive")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= to "") "Invalid target")
    (token-in::enforce-unit amount-in-total)
    (token-in::enforce-unit amount-in-min)
    (token-other::enforce-unit amount-other-min)
    (let*
      ( (order-ok (exchange.is-canonical token-in token-other))
        (p (exchange.get-pair token-in token-other))
        (pair-account (at 'account p))
        (reserve-in (exchange.reserve-for p token-in))
        (swap-amount-in (exchange.truncate token-in (get-one-sided-liquidity-swap-amount reserve-in amount-in-total)))
        (amount-in-liq (exchange.truncate token-in (- amount-in-total swap-amount-in)))
      )
      (enforce (>= amount-in-liq amount-in-min) "Insufficient A amount")
      (let* ;; perform the swap
        ( (swap-result (exchange.swap-exact-in swap-amount-in 0.0 [token-in token-other] sender sender sender-guard))
          (amount-other (at 'amount (at 1 swap-result)))
        )
        (enforce (>= amount-other amount-other-min) "Insufficient B amount")
        ;; add the liquidity as normal
        (if order-ok
            (add-liquidity token-in token-other amount-in-liq amount-other amount-in-min amount-other-min sender to to-guard)
            (add-liquidity token-other token-in amount-other amount-in-liq amount-other-min amount-in-min sender to to-guard)
        )
      )
    )
  )

  (defun add-liquidity:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      amountA-desired:decimal
      amountB-desired:decimal
      amountA-min:decimal
      amountB-min:decimal
      sender:string
      to:string
      to-guard:guard
    )
    "Wrapper around `exchange.add-liquidity`."
    (enforce-contract-unlocked)
    (enforce (and (and (and (> amountA-desired 0.0) (> amountB-desired 0.0)) (>= amountA-min 0.0)) (>= amountB-min 0.0)) "add-liquidity: Values must be positive")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= to "") "Invalid target")
    (enforce (exchange.is-canonical tokenA tokenB) "The tokens are not sorted")
    (let*
      ( (pair-key (exchange.get-pair-key tokenA tokenB))
        (pair-obj (exchange.get-pair tokenA tokenB))
        (total-liquidity-supply-before-add (tokens.total-supply pair-key))
        (liquidity-account (get-liquidity-account tokenA tokenB))
        (liquidity-account-name (at 'account liquidity-account))
        (liquidity-account-guard (at 'guard liquidity-account))
        (liquidity-position-key (get-liquidity-position-key tokenA tokenB to))
        (add-liquidity-result
          (exchange.add-liquidity tokenA tokenB amountA-desired amountB-desired amountA-min amountB-min sender liquidity-account-name liquidity-account-guard))
        (tokenA-reserves (exchange.reserve-for pair-obj tokenA))
        (tokenB-reserves (exchange.reserve-for pair-obj tokenB))
      )
      (bind add-liquidity-result {'amount0 := amountA, 'amount1 := amountB, 'liquidity := liquidity}
        (enforce (<= amountA amountA-desired) "sanity check: should not have added more than the desired amount")
        (enforce (<= amountB amountB-desired) "sanity check: should not have added more than the desired amount")
        (with-default-read liquidity-positions liquidity-position-key
          { 'pair: ""
          , 'account: ""
          , 'tokenA-pooled: 0.0
          , 'tokenB-pooled: 0.0
          , 'liquidity-tokens: 0.0
          }
          { 'pair := pair
          , 'account := account
          , 'liquidity-tokens := liquidity-tokens
          , 'tokenA-pooled := tokenA-pooled
          , 'tokenB-pooled := tokenB-pooled
          }
          ;; here we need to check if the liquidity position already exists (and we need to adjust its values) or if we need
          ;; to create a new position
          (if (!= pair pair-key) ;; pair will be "" if no prior liquidity position exists
            (if (= total-liquidity-supply-before-add 0.0) ;; when total liquidity is zero, the initial add-liquidity call locks up MINIMUM_LIQUIDITY, which works like a "fee" to the pooled amount
                (let ((real-share (/ liquidity (+ liquidity exchange.MINIMUM_LIQUIDITY))))
                  (insert liquidity-positions liquidity-position-key
                    { 'key: liquidity-position-key
                    , 'pair: pair-key
                    , 'account: to
                    , 'guard: to-guard
                    , 'liquidity-tokens: liquidity
                    , 'tokenA-pooled: (exchange.truncate tokenA (* real-share amountA))
                    , 'tokenB-pooled: (exchange.truncate tokenB (* real-share amountB))
                    , 'updated-at: (at 'block-time (chain-data))
                    , 'settled-multiplier: 0.0
                    , 'settled-liquidity-for-fees: 0.0
                    }))
                ;; in this case, the liquidity position is new, but liquidity already exists: add as normal with default values
                (insert liquidity-positions liquidity-position-key
                  { 'key: liquidity-position-key
                  , 'pair: pair-key
                  , 'account: to
                  , 'guard: to-guard
                  , 'liquidity-tokens: liquidity
                  , 'tokenA-pooled: amountA
                  , 'tokenB-pooled: amountB
                  , 'updated-at: (at 'block-time (chain-data))
                  , 'settled-multiplier: 0.0
                  , 'settled-liquidity-for-fees: 0.0
                  })
                )
            ;; in the following cases the liquidity position of the user already exists
            (if (= liquidity-tokens 0.0) ;; liquidity-tokens will be 0.0 if the user removed all their liquidity previously, in this case just update the amounts directly
                (update liquidity-positions liquidity-position-key
                  { 'tokenA-pooled: amountA
                  , 'tokenB-pooled: amountB
                  , 'liquidity-tokens: liquidity
                  })
                (let* ( ;; in this case, we're updating an existing liquidity position with non-zero amounts,
                        ;; so we need to IL-adjust the stored values before adding the new amounts
                       (tokenA-initial-pooled-adjusted (exchange.truncate tokenA (sqrt (* (* tokenA-pooled tokenB-pooled) (/ tokenA-reserves tokenB-reserves)))))
                       (tokenB-initial-pooled-adjusted (exchange.truncate tokenB (sqrt (* (* tokenA-pooled tokenB-pooled) (/ tokenB-reserves tokenA-reserves)))))
                       )
                  (update liquidity-positions liquidity-position-key
                    { 'tokenA-pooled: (+ tokenA-initial-pooled-adjusted amountA)
                    , 'tokenB-pooled: (+ tokenB-initial-pooled-adjusted amountB)
                    , 'liquidity-tokens: (+ liquidity-tokens liquidity)
                    })
                  )
                )
          )
        )
        ;; also update the liquidity account balance for the pair
        (with-read liquidity-accounts pair-key
          { 'liquidity-tokens := previous-total }
          (update liquidity-accounts pair-key { 'liquidity-tokens: (+ previous-total liquidity) }))
        ;; return the add-liquidity result
        add-liquidity-result
      )
    )
  )

  (defun dump-positions () ;; CLEANUP: testing function, do we want to remove?
    (select liquidity-positions (constantly true)))

  (defun dump-liquidity () ;; CLEANUP: testing function, do we want to remove?
    (select liquidity-accounts (constantly true)))

  (defun withdraw-claim:string
    ( sender:string
      request-id:string
    )
    "Allows users to withdraw their boosted KDX rewards."
    (enforce-contract-unlocked)
    (with-read reward-claim-requests request-id
      { 'status := status
      , 'pair-key := pair-key
      , 'to := to
      , 'to-guard := to-guard
      , 'tokenA-fees := tokenA-fees
      , 'tokenB-fees := tokenB-fees
      , 'tokenA-observations := tokenA-observations
      , 'tokenB-observations := tokenB-observations
      , 'total-kdx-swapped := total-kdx-swapped
      , 'fee-multiplier := fee-multiplier
      , 'settled-tokenA-fees := settled-tokenA-fees
      , 'settled-tokenB-fees := settled-tokenB-fees
      , 'settled-multiplier := settled-multiplier
      , 'end-time := end-time
      }
      ;; check that the user can actually claim the request
      (enforce (!= status 'CLAIMED) "This request has already been claimed")
      (enforce (= status 'PENDING-CLAIM) "Your request is not ready yet")
      (enforce (>= (at 'block-time (chain-data)) end-time) "Your request is not ready yet") ;; not strictly necessary but just in case
      (with-read pending-requests sender { 'requests := requests }
        (enforce (contains request-id requests) "The request informed is not from the sender informed"))

      (with-read trading-pairs pair-key
        { 'tokenA-base-path := tokenA-base-path, 'tokenB-base-path := tokenB-base-path }
        (let* ;; get the TWAP of tokenA->KDX and tokenB->KDX
          ( ;; here we want to only calculate the average price if necessary. if the path is not longer than 1, then there is nothing to calculate and the price is 1.0
            (tokenA-avg-price (if (> (length tokenA-base-path) 1) (get-average-price (at 0 tokenA-observations) (at 1 tokenA-observations)) 1.0))
            (tokenB-avg-price (if (> (length tokenB-base-path) 1) (get-average-price (at 0 tokenB-observations) (at 1 tokenB-observations)) 1.0))
            ;; get the KDX amounts by multiplying the fee amounts by the TWAPs
            (tokenA-kdx-amt (* tokenA-avg-price tokenA-fees))
            (tokenB-kdx-amt (* tokenB-avg-price tokenB-fees))
            (settled-tokenA-kdx-amt (* tokenA-avg-price settled-tokenA-fees))
            (settled-tokenB-kdx-amt (* tokenB-avg-price settled-tokenB-fees))
            ;; calculate the final amounts by subtracting what we got from swapping the fees
            (base-token:module{fungible-v2} (get-base-token))
            (final-amount (* fee-multiplier (+ tokenA-kdx-amt tokenB-kdx-amt)))
            (settled-final-amount (* settled-multiplier (+ settled-tokenA-kdx-amt settled-tokenB-kdx-amt)))
            ;; if, for some reason, total-kdx-swapped is larger than the final amount, use total-kdx-swapped instead
            (full-final-amount (exchange.truncate base-token (max (+ final-amount settled-final-amount) total-kdx-swapped)))
            (mint-amount (exchange.truncate base-token (max 0.0 (- full-final-amount total-kdx-swapped))))
          )
          ;; final-amount: total boosted fees with the current multiplier
          ;; settled-final-amount: total settled boosted fees with the old multiplier
          ;; full-final-amount: total amount of KDX the user will receive
          ;; mint-amount: total amount of KDX that needs to be minted
          (enforce (= full-final-amount (+ mint-amount total-kdx-swapped)) "Invariant violation")

          (if (> total-kdx-swapped 0.0)
              (let ((dummy 'dumb))
                ;; transfer total-kdx-swapped out from the bank to the user
                (install-capability (base-token::TRANSFER WRAPPER_KDX_BANK to total-kdx-swapped))
                (with-capability (BANK_ACCESS (format-token base-token))
                  (base-token::transfer-create WRAPPER_KDX_BANK to to-guard total-kdx-swapped))
              )
              true)

          (if (> mint-amount 0.0)
              (let ((dummy 'dumb))
                ;; transfer mint-amount out from the mint bank to the user
                (install-capability (base-token::TRANSFER WRAPPER_KDX_MINT_BANK to mint-amount))
                (with-capability (BANK_ACCESS (format-token base-token))
                  (base-token::transfer-create WRAPPER_KDX_MINT_BANK to to-guard mint-amount))
              )
              true)

          ;; update request to CLAIMED status
          (update reward-claim-requests request-id { 'status: 'CLAIMED })
          ;; remove the request-id from the pending requests table entries
          (with-read pending-requests sender { 'requests := requests }
            (update pending-requests sender { 'requests: (remove-elem-from-list request-id requests) }))

          (format "You have successfully claimed {} KDX in rewards" [full-final-amount])
        )
      )
    )
  )

  (defun get-average-price:decimal
    ( initial:object{exchange.observation}
      final:object{exchange.observation}
    )
    "Utility function for calculating the TWAP between two oracle observations performed."
    (enforce (> (at 'timestamp final) (at 'timestamp initial)) "get-average-price: final timestamp needs to be greater than initial")
    (let*
      ( (time-delta (diff-time (at 'timestamp final) (at 'timestamp initial)))
        (price-delta (- (at 'price final) (at 'price initial)))
      )
      (enforce (> time-delta 0.0) "get-average-price: time-delta needs to be positive")
      (enforce (> price-delta 0.0) "get-average-price: prive-delta needs to be positive")
      (exchange.truncate (get-base-token) (/ price-delta time-delta))
    )
  )

  (defun claim-request-needs-processing:bool
    ( request-id:string
    )
    (enforce-contract-unlocked)
    (with-read reward-claim-requests request-id { 'status := status, 'end-time := end-time }
      (cond
        ( (= status 'PENDING-REMOVE) true)
        ( (= status 'PENDING-SWAP-A) true)
        ( (= status 'PENDING-SWAP-B) true)
        ( (= status 'PENDING-TWAP) (>= (at 'block-time (chain-data)) end-time))
        false
      )
    )
  )

  (defun process-claim-request-if-necessary:bool
    ( request-id:string
    )
    "Advances a single claim request along the necessary steps."
    (enforce-contract-unlocked)
    (with-capability (OPS)
      (with-read reward-claim-requests request-id
        { 'status := status
        , 'tokenA-fees := tokenA-fees
        , 'tokenB-fees := tokenB-fees
        , 'settled-tokenA-fees := settled-tokenA-fees
        , 'settled-tokenB-fees := settled-tokenB-fees
        , 'tokenA-observations := tokenA-observations
        , 'tokenB-observations := tokenB-observations
        , 'pair-key := pair-key
        , 'liquidity-position-key := liquidity-position-key
        , 'end-time := end-time
        }
        (cond
          ( (= status 'PENDING-REMOVE) ;; need to remove the settled fee liquidity
            (with-read trading-pairs pair-key { 'tokenA := tokenA, 'tokenB := tokenB, 'account := wrapper-pair-account, 'guard := pair-guard }
              (with-read liquidity-positions liquidity-position-key
                { 'settled-multiplier := settled-multiplier
                , 'settled-liquidity-for-fees := settled-liquidity
                } ;; in this step, we remove the full settled fee liquidity amount for the user unconditionally
                ;; even if the user only removed a tiny bit of liquidity for KDX rewards, we will always remove the
                ;; old settled amounts here to make things simpler
                (if (> settled-liquidity 0.0)
                    (let*
                      ( (liquidity-account (get-liquidity-account tokenA tokenB))
                        (liquidity-account-name (at 'account liquidity-account))
                        (exchange-pair (exchange.get-pair tokenA tokenB))
                        (exchange-pair-account (at 'account exchange-pair))
                      )
                      ;; withdraw the settled-liquidity
                      (install-capability (tokens.TRANSFER pair-key liquidity-account-name exchange-pair-account settled-liquidity))
                      (let*
                        ( (remove-result
                            (with-capability (LIQUIDITY_ACCESS pair-key)
                              (exchange.remove-liquidity tokenA tokenB settled-liquidity 0.0 0.0 liquidity-account-name wrapper-pair-account pair-guard)))
                          (tokenA-is-token0 (exchange.is-leg0 exchange-pair tokenA))
                          (tokenA-amount (if tokenA-is-token0 (at 'amount0 remove-result) (at 'amount1 remove-result)))
                          (tokenB-amount (if tokenA-is-token0 (at 'amount1 remove-result) (at 'amount0 remove-result)))
                        )
                        ;; update the claim request data with the withdrawn amounts
                        (update reward-claim-requests request-id
                          { 'status: 'PENDING-SWAP-A
                          , 'settled-tokenA-fees: tokenA-amount
                          , 'settled-tokenB-fees: tokenB-amount
                          , 'settled-multiplier: settled-multiplier })
                        ;; update the user's liquidity position
                        (update liquidity-positions liquidity-position-key
                          { 'updated-at: (at 'block-time (chain-data))
                          , 'settled-multiplier: 0.0
                          , 'settled-liquidity-for-fees: 0.0 })
                      )
                    )
                    ;; if we don't have settled-liquidity, just update the claim status
                    (update reward-claim-requests request-id { 'status: 'PENDING-SWAP-A })
                )
                true
              )
            )
          )
          ( (= status 'PENDING-SWAP-A) ;; swap tokens for KDX and put them in the wrapper bank
            (with-read trading-pairs pair-key
              { 'account := pair-account, 'tokenA := tokenA, 'tokenA-base-path := tokenA-base-path }
              ;; swap both the regular and the settled fees for KDX and store the total output amount in the claim request data
              (let ((total-tokenA-fees (+ tokenA-fees settled-tokenA-fees)))
                (with-capability (FEE_BUYBACK tokenA total-tokenA-fees)
                  (with-capability (RESERVE_ACCESS pair-key)
                    (let ((total-swapped (swap-fees-for-base-and-bank tokenA total-tokenA-fees tokenA-base-path pair-account)))
                      (update reward-claim-requests request-id
                        { 'status: 'PENDING-SWAP-B
                        , 'total-kdx-swapped: total-swapped
                        })
                      true
                    )
                  )
                )
              )
            )
          )
          ( (= status 'PENDING-SWAP-B) ;; swap tokens for KDX and put them in the wrapper bank
            (with-read trading-pairs pair-key
              { 'account := pair-account, 'tokenB:= tokenB, 'tokenB-base-path := tokenB-base-path }
              ;; swap both the regular and the settled fees for KDX and store the total output amount in the claim request data
              (let ((total-tokenB-fees (+ tokenB-fees settled-tokenB-fees)))
                (with-capability (FEE_BUYBACK tokenB total-tokenB-fees)
                  (with-capability (RESERVE_ACCESS pair-key)
                    (let ((total-swapped (swap-fees-for-base-and-bank tokenB total-tokenB-fees tokenB-base-path pair-account)))
                      (with-read reward-claim-requests request-id { 'total-kdx-swapped := total-kdx-swapped-prev }
                        (update reward-claim-requests request-id
                          { 'status: 'PENDING-TWAP
                          , 'total-kdx-swapped: (+ total-swapped total-kdx-swapped-prev)
                          })
                        true
                      )
                    )
                  )
                )
              )
            )
          )
          ( (= status 'PENDING-TWAP) ;; measure TWAP endpoint if end-time has been reached
            (if (>= (at 'block-time (chain-data)) end-time)
                (with-read trading-pairs pair-key { 'tokenA-base-path := tokenA-path, 'tokenB-base-path := tokenB-path }
                  (let
                    ( (tokenA-obs (get-oracle-time-cumulative-price tokenA-path))
                      (tokenB-obs (get-oracle-time-cumulative-price tokenB-path))
                    )
                    ;; update the claim request data
                    (update reward-claim-requests request-id
                      { 'status: 'PENDING-CLAIM
                      , 'tokenA-observations: (+ tokenA-observations tokenA-obs)
                      , 'tokenB-observations: (+ tokenB-observations tokenB-obs)
                      })
                    true
                  )
                )
                false))
          ( (= status 'PENDING-CLAIM) ;; do nothing; user can claim their rewards
            false)
          ( (= status 'CLAIMED) ;; do nothing; user already claimed their rewards
            false)
          false
        )
      )
    )
  )

  (defun swap-fees-for-base-and-bank:decimal
    ( token:module{fungible-v2}
      token-fees:decimal
      token-base-path:[module{fungible-v2}]
      from:string
    )
    "Internal function for swapping the specified amount of token for KDX and store the result in WRAPPER_KDX_BANK."
    (enforce-contract-unlocked)
    (require-capability (FEE_BUYBACK token token-fees))
    (with-capability (SWAPPING_FEES)
      (let*
        ( (token-fees-truncated (exchange.truncate token token-fees))
          (total-swapped (swap-for-base token-base-path token-fees-truncated from WRAPPER_KDX_BANK (create-bank-guard)))
        )
        total-swapped
      )
    )
  )

  (defun swap-for-base:decimal
    ( token-path:[module{fungible-v2}]
      amount-in:decimal
      from:string
      to:string
      to-guard:guard
    )
    "Internal function for swapping the specified token into KDX, or, if it's already KDX, just transfers the amount."
    (enforce-contract-unlocked)
    (require-capability (SWAPPING_FEES))
    (let
      ( (len (length token-path))
        (base-token:module{fungible-v2} (get-base-token))
        (token-in:module{fungible-v2} (at 0 token-path))
      )
      (enforce (tokens-equal base-token (at (- len 1) token-path)) "Path must end at base-token")
      (if (= len 1)
        (at 2 [ ;; in this case, the path is only [KDX], so just transfer the amount and return
          (install-capability (base-token::TRANSFER from to amount-in))
          (base-token::transfer-create from to to-guard amount-in)
          amount-in
        ])
        ;; otherwise, perform the `swap-exact-in` call with the given swap path
        (let*
          ( (token-out-first-hop:module{fungible-v2} (at 1 token-path))
            (swap-target-account (at 'account (exchange.get-pair token-in token-out-first-hop)))
          )
          (install-capability (token-in::TRANSFER from swap-target-account amount-in))
          (let ((swap-result (exchange.swap-exact-in amount-in 0.0 token-path from to to-guard)))
            (enforce (= (format "{}" [base-token]) (at 'token (at (- len 1 ) swap-result))) "Swap output must be base-token")
            (at 'amount (at (- len 1) swap-result))
          )
        )
      )
    )
  )

  (defun get-user-pending-requests:[string]
    ( account:string )
    (at 'requests (read pending-requests account)))

  (defun get-user-pending-requests-info:[object]
    ( account:string )
    (map (get-request-info) (get-user-pending-requests account)))

  (defun get-request-info:object
    ( request-id:string )
    (with-read reward-claim-requests request-id
      { 'pair-key := pair-key
      , 'start-time := start-time
      , 'end-time := end-time
      , 'tokenA-fees := tokenA-fees
      , 'tokenB-fees := tokenB-fees
      , 'tokenA-observations := tokenA-observations
      , 'tokenB-observations := tokenB-observations
      , 'fee-multiplier := fee-multiplier
      , 'settled-tokenA-fees := settled-tokenA-fees
      , 'settled-tokenB-fees := settled-tokenB-fees
      , 'settled-multiplier := settled-multiplier
      }
      (with-read trading-pairs pair-key
        { 'tokenA-base-path := tokenA-base-path
        , 'tokenB-base-path := tokenB-base-path
        , 'tokenA := tokenA
        , 'tokenB := tokenB
        }
        (let*
          ( (now-time (at 'block-time (chain-data)))
            (tokenA-spot-kdx-price (exchange.get-spot-price-for-path tokenA-base-path))
            (tokenB-spot-kdx-price (exchange.get-spot-price-for-path tokenB-base-path))
            (has-observed-twap (and (> (length tokenA-observations) 1) (> (length tokenB-observations) 1)))
            (tokenA-twap (if (and (> (length tokenA-base-path) 1) has-observed-twap) (get-average-price (at 0 tokenA-observations) (at 1 tokenA-observations)) 1.0))
            (tokenB-twap (if (and (> (length tokenB-base-path) 1) has-observed-twap) (get-average-price (at 0 tokenB-observations) (at 1 tokenB-observations)) 1.0))
            (tokenA-kdx-price (if has-observed-twap tokenA-twap tokenA-spot-kdx-price))
            (tokenB-kdx-price (if has-observed-twap tokenB-twap tokenB-spot-kdx-price))
            (estimated-fees-kdx (+ (* tokenA-fees tokenA-kdx-price) (* tokenB-fees tokenB-kdx-price)))
            (estimated-settled-fees-kdx (+ (* settled-tokenA-fees tokenA-kdx-price) (* settled-tokenB-fees tokenB-kdx-price)))
            (total-estimated-unboosted-fees-kdx (+ estimated-fees-kdx estimated-settled-fees-kdx))
            (combined-multiplier (+ (* (/ estimated-fees-kdx total-estimated-unboosted-fees-kdx) fee-multiplier) (* (/ estimated-settled-fees-kdx total-estimated-unboosted-fees-kdx) settled-multiplier)))
            (estimated-kdx (exchange.truncate (get-base-token) (+ (* estimated-fees-kdx fee-multiplier) (* estimated-settled-fees-kdx settled-multiplier))))
          )
          { 'request-id: request-id
          , 'remaining-time: (max 0.0 (diff-time end-time now-time))
          , 'creation-time: start-time
          , 'claim-time: end-time
          , 'estimated-kdx: estimated-kdx
          , 'multiplier: combined-multiplier
          , 'tokenA: tokenA
          , 'tokenB: tokenB
          , 'has-observed-price: has-observed-twap
          , 'tokenA-observed-price: (if has-observed-twap tokenA-twap 0.0)
          , 'tokenB-observed-price: (if has-observed-twap tokenB-twap 0.0)
          }
        )
      )
    )
  )

  (defun compute-remaining-time:decimal (request-id:string)
    (let ((now-time (at 'block-time (chain-data))))
      (with-read reward-claim-requests request-id { 'end-time := end-time }
        (max 0.0 (diff-time end-time now-time)))))

  (defun compute-elapsed-time:decimal (request-id:string)
    (let ((now-time (at 'block-time (chain-data))))
      (with-read reward-claim-requests request-id { 'start-time := start-time }
        (min BOOSTED_REWARD_VESTING_TIME (diff-time now-time start-time)))))

  (defun is-reward-request-claimable:bool (request-id:string)
    (with-read reward-claim-requests request-id { 'status := status }
      (if (= status 'PENDING-CLAIM) true false)))

  (defun get-all-reward-requests:[string] ()
    (keys reward-claim-requests))

  (defun init (initial-lock:bool)
    (insert contract-lock CONTRACT_LOCK_KEY {'lock: initial-lock})
    (let ((base-token:module{fungible-v2} (get-base-token)))
      (base-token::create-account WRAPPER_KDX_BANK (create-bank-guard))
      (base-token::create-account WRAPPER_KDX_MINT_BANK (create-bank-guard))
    )
  )
)

(if (= (read-integer 'upgrade) 0)
    [ ;; deploying from scratch: create all tables
      (create-table contract-lock)
      (create-table trading-pairs)
      (create-table liquidity-positions)
      (create-table liquidity-accounts)
      (create-table reward-claim-requests)
      (create-table pending-requests)
      (kaddex.kdx.set-contract-lock false)
      (init (read-msg 'initial-lock))
      (kaddex.kdx.set-contract-lock true)
    ]
    (if (= (read-integer 'upgrade) 1)
        [ ;; upgrade from v1 (original devnet deploy) to v2 -- no schema changes
          "upgrade complete"
        ]
        [(enforce false (format "Invalid upgrade field: {}" [(read-msg 'upgrade)]))]))
