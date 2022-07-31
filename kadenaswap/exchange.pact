;; This file contains the core module for the exchange.
;;
;; This is a higher level implementation than the equivalent
;; UniswapV2Pair contract, since this single module can keep
;; track of the state of all pairs using the `pairs` table,
;; there is no need for multiple modules.
;;
;; Main user-facing functions are `add-liquidity`,
;; `remove-liquidity`, `swap-exact-in` and `swap-exact-out`.
;; Even though it's low level, users can still call the
;; `swap` function directly as well.

;(enforce-pact-version "4.3")

(namespace (read-msg 'ns))

(module exchange GOVERNANCE

  @model
  [

   ;; prop-pairs-write-guard
   ;; guard is never enforced, but this allows enumeration of
   ;; every write, and forward security for newly-added functions.
   (property
    (forall (k:string)
     (when (row-written pairs k)
       (row-enforced pairs 'guard k)))
    { 'except:
      [ create-pair      ;; unguarded (insert semantics)
        add-liquidity    ;; prop-increase-liquidity
        remove-liquidity ;; prop-decrease-liquidity
        swap-exact-in    ;; prop-increase-liquidity
        swap-exact-out   ;; prop-increase-liquidity
        swap             ;; prop-increase-liquidity
        swap-pair        ;; PRIVATE
        swap-alloc       ;; PRIVATE
        update-reserves  ;; PRIVATE
        update-k         ;; PRIVATE
      ] } )


   ;;prop-increase-liquidity
   ;;computes constant-product variance
   (defproperty increase-liquidity
     ( amount0:decimal
       amount1:decimal )
    (forall (k:string)
     (when (row-written pairs k)
      (<= (* (at 'reserve (at 'leg0 (read k)))
             (at 'reserve (at 'leg1 (read k))))
          (* (+ amount0
               (at 'reserve (at 'leg0 (read k))))
             (+ amount1
               (at 'reserve (at 'leg1 (read k)))))))))

   ;;prop-decrease-liquidity
   ;;computes constant-product variance
   (defproperty decrease-liquidity
     ( amount0:decimal
       amount1:decimal )
    (forall (k:string)
     (when (row-written pairs k)
      (>= (* (at 'reserve (at 'leg0 (read k)))
             (at 'reserve (at 'leg1 (read k))))
          (* (+ amount0
               (at 'reserve (at 'leg0 (read k))))
             (+ amount1
               (at 'reserve (at 'leg1 (read k)))))))))

  ]

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'kaddex-exchange-admin)))

  (defcap OPS ()
    (enforce-guard (keyset-ref-guard 'kaddex-exchange-ops)))

  (defcap CREATE_PAIR
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      key:string
      account:string )
    " Pair-created event for TOKEN0 and TOKEN1 pairs with KEY liquidity token \
    \ and ACCOUNT on leg tokens."
    @event
    ;; dupes checked in 'get-pair-create'
    true)

  (defcap LIQUIDITY_RESERVE
      (pair-key:string)
    true)

  (defun enforce-liquidity-reserve:bool
      (key:string)
    (require-capability (LIQUIDITY_RESERVE key)))

  (defun create-liquidity-guard:guard
      (key:string)
    (create-user-guard (enforce-liquidity-reserve key)))

  (defcap PRIVATE_RESERVE
      (pair-key:string token:string)
    true)

  (defun enforce-private-reserve:bool
      (pair-key:string token:module{fungible-v2})
    (require-capability (PRIVATE_RESERVE pair-key (format-token token))))

  (defun create-reserve-guard:guard
      (pair-key:string token:module{fungible-v2})
    (create-user-guard (enforce-private-reserve pair-key token)))

  (defcap FEE_ACCOUNT (pair-key:string)
    true)

  (defun enforce-fee-access (pair-key:string)
    (require-capability (FEE_ACCOUNT pair-key)))

  (defun create-fee-guard:bool (pair-key:string)
    (create-user-guard (enforce-fee-access pair-key)))

  (defun enforce-issuing:bool
      ()
    (require-capability (ISSUING)))

  (defun create-issuing-guard:guard
      ()
    (create-user-guard (enforce-issuing)))

  ;; this guard always fails, we only use it for create-pair to store in the guard field of the pairs table
  ;; that field is now unused, and should not be used going forward
  (defun enforce-null:bool
      ()
    false)
  (defun create-null-guard:guard
      ()
    (create-user-guard (enforce-null)))

  (defun format-token:string (token:module{fungible-v2})
    (format "{}" [token]))

  (defcap ISSUING ()
    "Private defcap for issuing operations."
    true)

  (defcap SWAPPING ()
    "Private defcap for swapping operations."
    true)

  (defcap SWAP
    ( sender:string
      receiver:string
      in:decimal
      token-in:module{fungible-v2}
      out:decimal
      token-out:module{fungible-v2}
    )
    " Swap event debiting IN of TOKEN-IN from SENDER \
    \ for OUT of TOKEN-OUT on RECEIVER."
    @event
    true
  )

  (defcap DEBUG (message:string)
    @event
    true)

  (defcap UPDATE
    ( pair:string
      reserve0:decimal
      reserve1:decimal
    )
    "Private defcap updating reserves for PAIR to RESERVE0 and RESERVE1."
    @event
    true
  )

  (defcap MINT_FEE (pair:string) true)
  (defcap UPDATE_K (pair:string) true)

  (defcap OBSERVING ()
    "Private defcap for recording oracle observations."
    true)

  ;; This capability is used for being safe and trying to prevent reentrancy issues
  ;; by locking each pair inside a swap/add-liquidity/remove-liquidity function.
  ;; Because pact is Turing incomplete, and recursion is disallowed and detected,
  ;; we have not been able to produce a PoC of what a reetrancy exploit would look
  ;; like, since pact detects the recursion attempt (even if it's not infinite) and
  ;; prevents the code from running.
  (defcap MUTEX ()
    "Private defcap for obtaining pair mutex."
    true)

  ;; Define a simple global contract lock for pausing the contract if necessary.
  (defschema contract-lock-status
      lock:bool)
  (deftable contract-lock:{contract-lock-status})
  (defconst CONTRACT_LOCK_KEY 'lock)
  (defun enforce-contract-unlocked ()
    "Asserts that the contract is not in a paused state."
    (with-read contract-lock CONTRACT_LOCK_KEY { 'lock := lock }
      (enforce (not lock) "Contract is paused")))

  (defschema leg
      "Represents a token of the trading pair."
    token:module{fungible-v2} ;; which token it is
    reserve:decimal)          ;; the amount of tokens in the pair's reserves

  (defschema pair
      "State of a trading pair."
    leg0:object{leg}    ;; the first leg of the trading pair
    leg1:object{leg}    ;; the second leg of the trading pair
    account:string      ;; the account that holds the pair funds (same account name for both tokens)
    guard:guard         ;; the guard used for this account
    fee-account:string  ;; the account that will receive the liquidity tokens for the 0.05% exchange fee
    fee-guard:guard     ;; the guard used for the fee-account
    last-k:decimal      ;; the last value for k (i.e. (sqrt (* reserve0 reserve1)))
    locked:bool)        ;; whether the pair is currently locked or not (to prevent reentrancy)

  (deftable pairs:{pair})

  (defschema oracle
      "State for TWAP oracles."
    pair-key:string                       ;; the pair this state refers to
    tracked-paths:[[module{fungible-v2}]] ;; compound observation table keys this pair is a part of, e.g. [KDX KDA KBTC] for a KDX/KDA pair
    cumulative-price0:decimal             ;; time-cumulative price of token1 in token0 for the pair (directly)
    cumulative-price1:decimal             ;; time-cumulative price of token0 in token1 for the pair (directly)
    last-observed:time)                   ;; timestamp of the last recorded observation

  (deftable oracles-v2:{oracle})

  (defschema observation
      "A TWAP observation."
    timestamp:time  ;; when the observation was performed
    price:decimal)  ;; the time-cumulative price of the observation

  (deftable observations-v2:{observation})

  ;; amount of liquidity to set aside when performing the initial `add-liquidity` call
  ;; this is used for making sure the pair reserves can never be fully zero'd out when removing liquidity
  (defconst MINIMUM_LIQUIDITY 0.1)

  ;; the percentage fee we collect on every swap (0.3%)
  (defconst FEE 0.003)

  ;; the percentage of the total swap that is reserved by the exchange (0.05%, used for the staking program)
  (defconst FEE_RESERVED 0.0005)

  (defun init (initial-lock:bool)
    "Initialize the contract."
    (insert contract-lock CONTRACT_LOCK_KEY {'lock: initial-lock})
    (with-capability (ISSUING)
      (tokens.init-issuer (create-issuing-guard)))
  )

  (defun get-lock-account-principal (key:string)
    (create-principal (create-liquidity-guard key))
  )

  (defun set-contract-lock
    ( lock:bool )
    "Sets the contract paused state."
    (with-capability (OPS)
      (write contract-lock CONTRACT_LOCK_KEY {'lock: lock})
    )
  )

  (defun obtain-pair-lock:bool (pair:string)
    "Obtain reentrancy mutex for the given pair."
    (require-capability (MUTEX))
    (with-read pairs pair { 'locked := locked }
      (enforce (not locked) (format "Pair {} is locked" [pair]))
      (update pairs pair { 'locked: true })
      true))

  (defun release-pair-lock:bool (pair:string)
    "Release reentrancy mutex for the given pair."
    (require-capability (MUTEX))
    (with-read pairs pair { 'locked := locked }
      (enforce locked (format "Pair {} is unlocked" [pair]))
      (update pairs pair { 'locked: false })
      true))

  (defun dump-observations:[object] ;; CLEANUP: debugging only, remove later
    ( pair-key:string )
    (let*
      ( (oracle (read oracles-v2 pair-key ['tracked-paths]))
        (observation-keys (map (get-observation-key) (at 'tracked-paths oracle)))
        (try-get-observation (lambda (k) (with-default-read observations-v2 k { 'timestamp: (at 'block-time (chain-data)), 'price: 0.0 } { 'timestamp := ts, 'price := p } { 'path-key: k, 'timestamp: ts, 'price: p })))
      )
      (map (try-get-observation) observation-keys)
    )
  )

  (defun chunk-list-pairs
    ( lst )
    "Given a list like [1 2 3 4], returns [[1 2] [2 3] [3 4]]."
    (enforce (>= (length lst) 2) "list too short")
    (let
      ( (get-nth-pair (lambda (n) (take 2 (drop (- n 1) lst))))
      )
      (map (get-nth-pair) (enumerate 1 (- (length lst) 1)))
    )
  )

  (defun get-observation-key:string
    ( token-path:[module{fungible-v2}] )
    "Returns the key used for the observations table."
    (concat (map (lambda (t) (format "{}:" [t])) token-path))
  )

  (defun get-oracle-time-cumulative-price:object{observation}
    ( path:[module{fungible-v2}] )
    "Returns the last recorded observation for the given swap path."
    (read observations-v2 (get-observation-key path))
  )

  (defun observe-direct:object
    ( oracle:object{oracle}
      pair-key:string
    )
    "Performs the TWAP observation for a direct pair (only two tokens)."
    (require-capability (OBSERVING))
    (let*
      ( (pair (read pairs pair-key))
        (leg0 (at 'leg0 pair))
        (leg1 (at 'leg1 pair))
        (last-observed (at 'last-observed oracle))
        (last-cumulative-price0 (at 'cumulative-price0 oracle))
        (last-cumulative-price1 (at 'cumulative-price1 oracle))
        (block-time (at 'block-time (chain-data)))
        (time-delta (diff-time block-time last-observed))
        (reserve0 (at 'reserve leg0))
        (reserve1 (at 'reserve leg1))
        (price0 (try 0.0 (/ reserve0 reserve1)))
        (price1 (try 0.0 (/ reserve1 reserve0)))
        ;; TODO: round to the max of the precisions of the tokens instead of 8?
        (cumulative-price0 (round (+ last-cumulative-price0 (* time-delta price0)) 8))
        (cumulative-price1 (round (+ last-cumulative-price1 (* time-delta price1)) 8))
      )
      { 'timestamp: block-time
      , 'price0: cumulative-price0
      , 'price1: cumulative-price1 }
    )
  )

  (defun get-spot-price-for:decimal
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
    )
    "Returns the current spot price for a token pair. This function requires the token-in/token-out pair to exist."
    (let*
      ( (pair (get-pair token-in token-out))
        (leg0 (at 'leg0 pair))
        (leg1 (at 'leg1 pair))
        (reserve0 (at 'reserve leg0))
        (reserve1 (at 'reserve leg1))
      )
      (if (is-leg0 pair token-in)
          (try 0.0 (/ reserve1 reserve0)) ;; TODO: do we realy need this try here?
          (try 0.0 (/ reserve0 reserve1))
      )
      ;; TODO: should we round the result here?
    )
  )

  (defun get-spot-price-for-path:decimal
    ( token-path:[module{fungible-v2}]
    )
    "Returns the current spot price for a token path. This function requires each individual token pair in the path to exist."
    (if
      (<= (length token-path) 1)
      1.0
      (let*
        ( (token-path-chunks (chunk-list-pairs token-path))   ;; get all the intermediary pairs we need to check
          (get-price-helper (lambda (p) (get-spot-price-for (at 0 p) (at 1 p))))   ;; helper for getting the price of a step
          (token-prices (map (get-price-helper) token-path-chunks))   ;; list of all the token prices in the swap path
          (combined-price (fold (*) 1.0 token-prices))   ;; multiply the token prices together to get the composite price
        )
        combined-price
      )
    )
  )

  (defun observe-compound-path
    ( token-path:[module{fungible-v2}]
    )
    "Updates the observation for a given token path."
    (require-capability (OBSERVING))
    (let*
      ( (combined-price (get-spot-price-for-path token-path))   ;; get the compound spot price using the helper above
        (observation-key (get-observation-key token-path)) ;; get the observation key and current block time
        (block-time (at 'block-time (chain-data)))
      )
      (with-default-read observations-v2 observation-key
        { 'timestamp: block-time, 'price: 0.0 }
        { 'timestamp := last-observed, 'price := last-cumulative-price }
        (let*
          ( (time-delta (diff-time block-time last-observed))
            ;; add the new price multiplied by time passed since the last observation
            (new-cumulative-price (round (+ last-cumulative-price (* time-delta combined-price)) 8))
            ;; TODO: can we round to more places?
          )
          (write observations-v2 observation-key { 'timestamp: block-time, 'price: new-cumulative-price })
        )
      )
    )
  )

  (defun maybe-observe:bool
    ( pair-key:string
    )
    "Update (if necessary) all the relevant oracle information for a given pair. Returns whether there was an update."
    (require-capability (OBSERVING))
    (with-default-read oracles-v2 pair-key
      { 'pair-key: "" }
      { 'pair-key := oracle-pair-key }
      (if (= oracle-pair-key "") ;; if this is the first observation, create the table entry
        (write oracles-v2 pair-key
          { 'pair-key: pair-key
          , 'tracked-paths: []
          , 'cumulative-price0: 0.0
          , 'cumulative-price1: 0.0
          , 'last-observed: (at 'block-time (chain-data)) }
        ) "")
      (let*
        ( (block-time (at 'block-time (chain-data)))
          (oracle (read oracles-v2 pair-key))
          (last-observed (at 'last-observed oracle))
        )
        (if (and (!= oracle-pair-key "") (<= block-time last-observed)) false ;; if the last observation is too recent, do nothing
          (let* ;; otherwise, update the direct observation as well as call `observe-compound-path` on all the tracked paths.
            ( (new-direct-observation (observe-direct oracle pair-key))
              (cumulative-price0 (at 'price0 new-direct-observation))
              (cumulative-price1 (at 'price1 new-direct-observation))
            )
            (update oracles-v2 pair-key
              { 'cumulative-price0: cumulative-price0
              , 'cumulative-price1: cumulative-price1
              , 'last-observed: block-time })
            (map (observe-compound-path) (at 'tracked-paths oracle))
            true
          )
        )
      )
    )
  )

  (defun oracle-add-tracked-path ;; TODO: enforce path has more than 2 entries; we're already tracking
      ;; each pair's cumulative price separately (alternatively, we stop tracking it there and don't check it here)
    ( path:[module{fungible-v2}]
    )
    "Add a new tracked token swap path for the oracle."
    (with-capability (OPS)
      (let
        ( (all-pairs (chunk-list-pairs path)) ;; get a list of all pairs that affect the path
          (add-path (lambda (p) (let ((pair-key (get-pair-key (at 0 p) (at 1 p)))) (add-tracked-path pair-key path))))
        )
        (map (add-path) all-pairs) ;; add the path to each pairs' tracked-paths list
      )
    )
  )

  (defun is-path-tracked:bool
    ( path:[module{fungible-v2}]
    )
    "Checks whether a given path is being tracked by the TWAP oracle."
    (let*
      ( (all-pairs (chunk-list-pairs path))
        (compare-paths (lambda (a b) (= (format "{}" [a]) (format "{}" [b]))))
        (alt-contains (lambda (lst elm) (> (length (filter (lambda (x) (compare-paths x elm)) lst)) 0)))
        (contains-path (lambda (p) (let ((pair-key (get-pair-key (at 0 p) (at 1 p)))) (with-default-read oracles-v2 pair-key { 'tracked-paths: [] } { 'tracked-paths := tracked-paths } (alt-contains tracked-paths path)))))
      )
      (fold (and) true (map (contains-path) all-pairs))
    )
  )

  (defun add-tracked-path
    ( pair-key:string
      new-path:[module{fungible-v2}]
    )
    "Add a given path to the pairs' `tracked-path` variable."
    (enforce (>= (length new-path) 2) "Path too short")
    (with-capability (OPS)
      (let ((all-pairs-exist (map (lambda (lst) (pair-exists (at 0 lst) (at 1 lst))) (chunk-list-pairs new-path))))
        (enforce (fold (and) true all-pairs-exist) "some intermediary pair does not exist")
      ) ;; make sure every pair in the path exists
      (with-default-read oracles-v2 pair-key
        { 'pair-key: "", 'tracked-paths: [] }
        { 'pair-key := oracle-pair-key, 'tracked-paths := tracked-paths }
        (if (= oracle-pair-key "") ;; if the pair has no oracle entry, create it, otherwise update it
          (write oracles-v2 pair-key
            { 'pair-key: pair-key
            , 'tracked-paths: [new-path]
            , 'cumulative-price0: 0.0
            , 'cumulative-price1: 0.0
            , 'last-observed: (at 'block-time (chain-data)) }
          )
          (update oracles-v2 pair-key { 'tracked-paths: (+ tracked-paths [new-path]) })
        )
      )
    )
  )

  (defun get-pair:object{pair}
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Returns pair data."
    (read pairs (get-pair-key tokenA tokenB)))

  (defun pair-exists:bool
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Returns whether a pair exists."
    (with-default-read pairs
      (get-pair-key tokenA tokenB)
      { 'account: "" }
      { 'account := a }
      (> (length a) 0))
  )

  (defun update-reserves
    ( p:object{pair}
      pair-key:string
      reserve0:decimal
      reserve1:decimal
    )
    "Internal function for updating the `leg0` and `leg1` reserves of a pair."
    (with-capability (OBSERVING)
      (maybe-observe pair-key) ;; if necessary, update the oracle data for the pair
    )
    (require-capability (UPDATE pair-key reserve0 reserve1))
    (update pairs pair-key
      { 'leg0: { 'token: (at 'token (at 'leg0 p))
               , 'reserve: reserve0 }
      , 'leg1: { 'token: (at 'token (at 'leg1 p))
               , 'reserve: reserve1 }})
  )

  (defun mint-fee-manual (pair-key:string)
    "Manually mint the exchange fees."
    ;; this function is usually only called after an add or remove liquidity event,
    ;; but sometimes we may want to call it directly without needing to go through those functions.
    (with-capability (OPS)
      (with-capability (MINT_FEE pair-key)
        (mint-fee (get-pair-by-key pair-key))
      )
    )
  )

  (defun mint-fee
    ( p:object{pair}
    )
    "Mints liquidity tokens for the `fee-account` such that it controls `FEE_RESERVED` of the total swap amount."
    ;; this function is equivalent to the uniswap _mintFee function
    (let*
      ( (token0:module{fungible-v2} (at 'token (at 'leg0 p)))
        (token1:module{fungible-v2} (at 'token (at 'leg1 p)))
        (reserve0 (at 'reserve (at 'leg0 p)))
        (reserve1 (at 'reserve (at 'leg1 p)))
        (key (get-pair-key token0 token1))
      )
      (require-capability (MINT_FEE key))
      (let*
        ( (current-k (sqrt (* reserve0 reserve1)))
          (last-k (at 'last-k p))
          (fee-account (at 'fee-account p))
          (fee-guard (at 'fee-guard p))
        )
        (if (and (> FEE_RESERVED 0.0) (> (at 'last-k p) 0.0))
          (let*
            ( (fee-denominator (- (/ FEE FEE_RESERVED) 1.0))
              (numerator (* (tokens.total-supply key) (- current-k last-k)))
              (denominator (+ (* fee-denominator current-k) last-k))
              (fee-liquidity (tokens.truncate key (/ numerator denominator)))
            )
            (if (> fee-liquidity 0.0)
              (with-capability (ISSUING)
                (mint key fee-account fee-guard fee-liquidity)) {})
          ) {}
        )
        ;; normally not needed, minimize damage if mint-fee is called but not
        ;; update-k afterwards (which should never happen)
        (with-capability (UPDATE_K key)
          (update-k key))
      )
    )
  )

  (defun add-liquidity:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      amountADesired:decimal
      amountBDesired:decimal
      amountAMin:decimal
      amountBMin:decimal
      sender:string
      to:string
      to-guard:guard
    )
    "Adds liquidity to an existing pair. The `to` account specified will receive the liquidity tokens."
    (enforce-contract-unlocked)
    (enforce (and (and (and (> amountADesired 0.0) (> amountBDesired 0.0)) (>= amountAMin 0.0)) (>= amountBMin 0.0)) "add-liquidity: Values must be positive")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= to "") "Invalid target")
    (tokenA::enforce-unit amountADesired) ;; enforce the informed amounts are in the correct precision
    (tokenB::enforce-unit amountBDesired)
    (with-capability (MUTEX) ;; obtain the mutex lock
      (obtain-pair-lock (get-pair-key tokenA tokenB)))
    (let*
      ( (p (get-pair tokenA tokenB))
        (reserveA (reserve-for p tokenA))
        (reserveB (reserve-for p tokenB))
        ;; calculate the actual amounts of liquidity that will be added to keep the reserve ratios
        (amounts
          (if (and (= reserveA 0.0) (= reserveB 0.0))
            [amountADesired amountBDesired]
            (let ((amountBOptimal (quote amountADesired reserveA reserveB)))
              (if (<= amountBOptimal amountBDesired)
                (let ((x (enforce (>= amountBOptimal amountBMin)
                           "add-liquidity: insufficient B amount")))
                  [amountADesired amountBOptimal])
                (let ((amountAOptimal (quote amountBDesired reserveB reserveA)))
                  (enforce (<= amountAOptimal amountADesired)
                    "add-liquidity: optimal A less than desired")
                  (enforce (>= amountAOptimal amountAMin)
                    "add-liquidity: insufficient A amount")
                  [amountAOptimal amountBDesired])))))
        (amountA (truncate tokenA (at 0 amounts)))
        (amountB (truncate tokenB (at 1 amounts)))
        (pair-account (at 'account p))
      )
      (with-capability (MINT_FEE (get-pair-key tokenA tokenB)) ;; if necessary, mint exchange fees
        (mint-fee p))
      ;; transfer the tokens from the user to the pair
      (tokenA::transfer sender pair-account amountA)
      (tokenB::transfer sender pair-account amountB)
      ;; mint the liquidity tokens to the user
      (let* ;; first we calculate the actual amounts transferred by calling `get-balance`
        ( (token0:module{fungible-v2} (at 'token (at 'leg0 p)))
          (token1:module{fungible-v2} (at 'token (at 'leg1 p)))
          (balance0 (token0::get-balance pair-account))
          (balance1 (token1::get-balance pair-account))
          (reserve0 (at 'reserve (at 'leg0 p)))
          (reserve1 (at 'reserve (at 'leg1 p)))
          (amount0 (- balance0 reserve0))
          (amount1 (- balance1 reserve1))
          (key (get-pair-key tokenA tokenB))
          ;; given the liquidity tokens' total supply, calculate the amount of liquidity we need to mint
          (totalSupply (tokens.total-supply key))
          (liquidity (tokens.truncate key
            (if (= totalSupply 0.0) ;; in this case, we need to mint MINIMUM_LIQUIDITY
              (with-capability (ISSUING)
                (mint key (get-lock-account-principal key) (create-liquidity-guard key) MINIMUM_LIQUIDITY)
                (- (sqrt (* amount0 amount1)) MINIMUM_LIQUIDITY))
              (let ((l0 (/ (* amount0 totalSupply) reserve0))
                    (l1 (/ (* amount1 totalSupply) reserve1))
                   )
                ;; here we take the minimum between l0 and l1
                (if (<= l0 l1) l0 l1)))))
        )
        ;; mint the liquidity for the user
        (enforce (> liquidity 0.0) "mint: insufficient liquidity minted")
        (with-capability (ISSUING)
          (mint key to to-guard liquidity))
        ;; update pair reserves and last-k value
        (with-capability (UPDATE key balance0 balance1)
          (update-reserves p key balance0 balance1))
        (with-capability (UPDATE_K key)
          (update-k key))
        ;; release the pair lock
        (with-capability (MUTEX)
          (release-pair-lock (get-pair-key tokenA tokenB)))
        ;; return the information to the user
        { "liquidity": liquidity
        , "supply": (tokens.total-supply key)
        , "amount0": amount0
        , "amount1": amount1
        }
      )
    )
  )

  (defun mint (token:string to:string guard:guard amount:decimal)
    "Internal function used for minting liquidity tokens."
    (require-capability (ISSUING))
    (install-capability (tokens.MINT token to amount))
    (tokens.mint token to guard amount)
  )

  (defun quote
    ( amountA:decimal
      reserveA:decimal
      reserveB:decimal
    )
    (enforce (> amountA 0.0) "quote: insufficient amount")
    (enforce (and (> reserveA 0.0) (> reserveB 0.0)) "quote: insufficient liquidity")
    (/ (* amountA reserveB) reserveA)
  )

  (defun update-k
    ( pair-key:string
    )
    "Update the `last-k` value of the pair."
    (require-capability (UPDATE_K pair-key))
    (let*
      ( (p (read pairs pair-key))
        (reserve0 (at 'reserve (at 'leg0 p)))
        (reserve1 (at 'reserve (at 'leg1 p)))
        (current-k (sqrt (* reserve0 reserve1)))
      )
      (update pairs pair-key
        { 'last-k: current-k })
    )
  )

  (defun remove-liquidity:object
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
      liquidity:decimal
      amountAMin:decimal
      amountBMin:decimal
      sender:string
      to:string
      to-guard:guard
    )
    "Removes liquidity from an existing pair. The `to` account specified will receive the tokens."
    (enforce-contract-unlocked)
    (enforce (and (and (> liquidity 0.0) (>= amountAMin 0.0)) (>= amountBMin 0.0)) "remove-liquidity: Values must be positive")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= to "") "Invalid target")
    (with-capability (MUTEX) ;; obtain the pair lock
      (obtain-pair-lock (get-pair-key tokenA tokenB)))
    (let* ( (p (get-pair tokenA tokenB))
            (pair-account (at 'account p))
            (pair-key (get-pair-key tokenA tokenB))
          )
      ;; if necessary, mint fee tokens
      (with-capability (MINT_FEE pair-key)
        (mint-fee p))
      ;; transfer liquidity tokens from the sender to the pair for burning
      (tokens.transfer pair-key sender pair-account liquidity)
      (let* ;; calculate current reserves and withdrawal amount
        ( (token0:module{fungible-v2} (at 'token (at 'leg0 p)))
          (token1:module{fungible-v2} (at 'token (at 'leg1 p)))
          (balance0 (token0::get-balance pair-account))
          (balance1 (token1::get-balance pair-account))
          (total-supply (tokens.total-supply pair-key))
          (amount0 (truncate token0 (/ (* liquidity balance0) total-supply)))
          (amount1 (truncate token1 (/ (* liquidity balance1) total-supply)))
          (canon (is-canonical tokenA tokenB))
        )
        ;; enforce values are sensible
        (enforce (and (> amount0 0.0) (> amount1 0.0))
          "remove-liquidity: insufficient liquidity burned")
        (enforce (>= (if canon amount0 amount1) amountAMin)
          "remove-liquidity: insufficient A amount")
        (enforce (>= (if canon amount1 amount0) amountBMin)
          "remove-liquidity: insufficient B amount")
        ;; burn the liquidity tokens received from the user
        (with-capability (ISSUING)
          (with-capability (LIQUIDITY_RESERVE pair-key)
            (burn pair-key pair-account liquidity)))
        ;; transfer both tokens to the user
        (install-capability (token0::TRANSFER pair-account to amount0))
        (with-capability (PRIVATE_RESERVE pair-key (format-token token0))
          (token0::transfer-create pair-account to to-guard amount0)
        )
        (install-capability (token1::TRANSFER pair-account to amount1))
        (with-capability (PRIVATE_RESERVE pair-key (format-token token1))
          (token1::transfer-create pair-account to to-guard amount1)
        )
        ;; update the reserves with the new balances and the last-k value
        (let
          ( (token0-balance (token0::get-balance pair-account))
            (token1-balance (token1::get-balance pair-account)))
          (with-capability (UPDATE pair-key token0-balance token1-balance)
            (update-reserves p pair-key token0-balance token1-balance))
          (with-capability (UPDATE_K pair-key)
            (update-k pair-key)))
        ;; release the pair lock
        (with-capability (MUTEX)
          (release-pair-lock (get-pair-key tokenA tokenB)))
        ;; return the withdrawn amounts
        { 'amount0: amount0
        , 'amount1: amount1
        }
      )
    )
  )

  (defun burn (token:string to:string amount:decimal)
    "Internal function used for burning liquidity tokens."
    (require-capability (ISSUING))
    (install-capability (tokens.BURN token to amount))
    (tokens.burn token to amount)
  )

  (defschema alloc
      "Internal schema representing information necessary for a swap hop."
    token-out:module{fungible-v2} ;; output token for this hop
    token-in:module{fungible-v2}  ;; input token for this hop (last hop's output token if multi-hop)
    out:decimal                   ;; output amount
    in:decimal                    ;; input amount
    idx:integer                   ;; hop index
    pair:object{pair}             ;; pair object for the hop
    path:[module{fungible-v2}])   ;; full swap token path

  (defun swap-exact-in
    ( amountIn:decimal
      amountOutMin:decimal
      path:[module{fungible-v2}]
      sender:string
      to:string
      to-guard:guard
    )
    "Swaps exactly `amountIn` using the token path `path`. Sends from `sender` and `to` receives the result tokens. Ensures that the final output amount is at least `amountOutMin`"
    (enforce-contract-unlocked)
    (enforce (>= (length path) 2) "swap-exact-in: invalid path")
    ;; fold over tail of path with dummy first value to compute outputs
    ;; assembles allocs in reverse order
    (let*
      ( (p0 (get-pair (at 0 path) (at 1 path)))
        (allocs
          (fold (compute-out)
            [ { 'token-out: (at 0 path)
              , 'token-in: (at 1 path)
              , 'out: amountIn
              , 'in: 0.0
              , 'idx: 0
              , 'pair: p0
              , 'path: path
              }]
            (drop 1 path)))
      )
      (enforce (>= (at 'out (at 0 allocs)) amountOutMin)
        (format "swap-exact-in: insufficient output amount {}" [(at 'out (at 0 allocs))]))
      ;; initial dummy is correct for initial transfer
      (with-capability (SWAPPING)
        (swap-pair sender to to-guard (reverse allocs)))
    )
  )

  (defun compute-out
    ( allocs:[object{alloc}]
      token-out:module{fungible-v2}
    )
    "Internal utility function for calculating the information needed for an individual swap-exact-out hop."
    (let*
      ( (head:object{alloc} (at 0 allocs))
        (token-in:module{fungible-v2} (at 'token-out head))
        (amountIn:decimal (at 'out head))
        (p (get-pair token-in token-out))
        ;; this calculation is equivalent to Uniswap's getAmountOut
        (reserveIn (reserve-for p token-in))
        (reserveOut (reserve-for p token-out))
        (amountInWithFee (* (- 1.0 FEE) amountIn))
        (numerator (* amountInWithFee reserveOut))
        (denominator (+ reserveIn amountInWithFee))
      )
      (+ [ { 'token-out: token-out
           , 'token-in: token-in
           , 'in: amountIn
           , 'out: (truncate token-out (/ numerator denominator))
           , 'idx: (+ 1 (at 'idx head))
           , 'pair: p
           , 'path: (drop 1 (at 'path head))
           } ]
         allocs)
    )
  )

  (defun swap-exact-out
    ( amountOut:decimal
      amountInMax:decimal
      path:[module{fungible-v2}]
      sender:string
      to:string
      to-guard:guard
    )
    "Swaps enough tokens to get exactly `amountOut` using the token path `path`. Sends from `sender` and `to` receives the result tokens. Ensures that the initial input amount is at most `amountInMax`"
    (enforce-contract-unlocked)
    (enforce (>= (length path) 2) "swap-exact-out: invalid path")
    ;; fold over tail of reverse path with dummy first value to compute inputs
    ;; assembles allocs in forward order
    (let*
      ( (rpath (reverse path))
        (path-len (length path))
        (pz (get-pair (at 0 rpath) (at 1 rpath)))
        (e:[module{fungible-v2}] [])
        (allocs
          (fold (compute-in)
            [ { 'token-out: (at 1 rpath)
              , 'token-in: (at 0 rpath)
              , 'out: 0.0
              , 'in: amountOut
              , 'idx: path-len
              , 'pair: pz
              , 'path: e
              }]
            (drop 1 rpath)))
        (allocs1 ;; drop dummy at end, prepend dummy for initial transfer
          (+ [  { 'token-out: (at 0 path)
                , 'token-in: (at 1 path)
                , 'out: (at 'in (at 0 allocs))
                , 'in: 0.0
                , 'idx: 0
                , 'pair: (at 'pair (at 0 allocs))
                , 'path: path
             } ]
             (take (- path-len 1) allocs)))
      )
      (enforce (<= (at 'out (at 0 allocs1)) amountInMax)
        (format "swap-exact-out: excessive input amount {}" [(at 'out (at 0 allocs1))]))
      (with-capability (SWAPPING)
        (swap-pair sender to to-guard allocs1))
    )
  )

  (defun compute-in
    ( allocs:[object{alloc}]
      token-in:module{fungible-v2}
    )
    "Internal utility function for calculating the information needed for an individual swap-exact-in hop."
    (let*
      ( (head:object{alloc} (at 0 allocs))
        (token-out:module{fungible-v2} (at 'token-in head))
        (amountOut:decimal (at 'in head))
        (p (get-pair token-in token-out))
        ;; this calculation is equivalent to Uniswap's getAmountIn
        (reserveIn (reserve-for p token-in))
        (reserveOut (reserve-for p token-out))
        (numerator (* reserveIn amountOut))
        (denominator (* (- reserveOut amountOut) (- 1.0 FEE)))
      )
      (+ [ { 'token-out: token-out
           , 'token-in: token-in
           , 'in: (ceiling (/ numerator denominator) (token-in::precision))
           , 'out: amountOut
           , 'idx: (- (at 'idx head) 1)
           , 'pair: p
           , 'path: (+ [token-out] (at 'path head))
           } ]
         allocs)
    )
  )



  (defun swap-pair
    ( sender:string
      to:string
      to-guard:guard
      allocs:[object{alloc}]
    )
    "Internal function used by both `swap-exact-in` and `swap-exact-out` for performing the individual swaps."
    (require-capability (SWAPPING))
    (let*
      ( (head:object{alloc} (at 0 allocs))
        (head-token:module{fungible-v2} (at 'token-out head))
        (account (at 'account (at 'pair head)))
        (out (at 'out head))
      )
      (head-token::transfer sender account out)
      (+ [ { 'token: (format "{}" [head-token])
           , 'amount: out } ]
        (map
          (swap-alloc
            (- (length allocs) 1)
            to
            to-guard)
          (drop 1 allocs)))
    )
  )

  (defun swap-alloc
    ( last:integer
      to:string
      guard:guard
      alloc:object{alloc}
    )
    "Internal function for performing a single swap hop using the information from the `alloc` object."
    (require-capability (SWAPPING))
    (let*
      ( (path (at 'path alloc))
        (is-last (= last (at 'idx alloc)))
        (next-pair
          (if is-last (at 'pair alloc) (get-pair (at 0 path) (at 1 path))))
        (recipient
          (if is-last to (at 'account next-pair)))
        (next-pair-key (get-pair-key (at 'token (at 'leg0 next-pair)) (at 'token (at 'leg1 next-pair))))
        (recip-guard
          (if is-last guard (create-reserve-guard next-pair-key (at 'token-out alloc))))
      )
      (swap noop-callable recipient recip-guard
        (at 'token-out alloc)
        (at 'out alloc)
        (at 'token-in alloc)))
  )

  (defun swap
    ( callable:module{swap-callable-v1}
      recipient:string
      recip-guard:guard
      token:module{fungible-v2}
      amount-out:decimal
      token-in:module{fungible-v2}
    )
    " Swap AMOUNT-OUT of TOKEN to RECIPIENT/RECIP-GUARD, \
    \ such that a corresponding transfer to TOKEN-IN, either \
    \ previously or during the execution of 'CALLABLE::swap-call', \
    \ will satisfy the constant-product invariant for the pair."
    (enforce-contract-unlocked)
    (with-capability (MUTEX) ;; acquire the pair lock
      (obtain-pair-lock (get-pair-key token token-in)))
    (let*
      ( (p (get-pair token token-in))
        (account (at 'account p))
        (reserve-out (reserve-for p token))
      )
      (enforce (> amount-out 0.0) "swap: insufficient output")
      (enforce (< amount-out reserve-out) "swap: insufficient liquidity")
      (enforce (!= recipient account) "swap: invalid TO")

      ;; fire swap event
      (install-capability (token::TRANSFER account recipient amount-out))
      (with-capability (PRIVATE_RESERVE (get-pair-key token token-in) (format-token token))
        (token::transfer-create account recipient recip-guard amount-out)
      )

      ;; perform the swap-callable call
      (callable::swap-call token-in token amount-out account recipient recip-guard)

      ;; verify if the invariants are still satisfied
      (let*
        ( (leg0 (at 'leg0 p))
          (leg1 (at 'leg1 p))
          (token0:module{fungible-v2} (at 'token leg0))
          (token1:module{fungible-v2} (at 'token leg1))
          (balance0 (token0::get-balance account))
          (balance1 (token1::get-balance account))
          (reserve0 (at 'reserve leg0))
          (reserve1 (at 'reserve leg1))
          (canon (is-leg0 p token))
          (amount0Out (if canon amount-out 0.0))
          (amount1Out (if canon 0.0 amount-out))
          (amount0In (if (> balance0 (- reserve0 amount0Out))
                        (- balance0 (- reserve0 amount0Out))
                        0.0))
          (amount1In (if (> balance1 (- reserve1 amount1Out))
                        (- balance1 (- reserve1 amount1Out))
                        0.0))
          (balance0adjusted (- balance0 (* amount0In FEE)))
          (balance1adjusted (- balance1 (* amount1In FEE)))
        )
        (enforce (or (> amount0In 0.0) (> amount1In 0.0))
          "swap: insufficient input amount")
        (enforce (>= (* balance0adjusted balance1adjusted)
                     (* reserve0 reserve1))
          (format "swap: K ({} < {})"
          [(* balance0adjusted balance1adjusted) (* reserve0 reserve1)]))
        ;; update the pair reserves with the new balances
        (with-capability (UPDATE (get-pair-key token0 token1) balance0 balance1)
          (with-capability
            (SWAP account recipient
              (if canon amount1In amount0In)
              token-in amount-out token)
            (update-reserves p
              (get-pair-key token0 token1) balance0 balance1)))
        ;; release the pair lock
        (with-capability (MUTEX)
          (release-pair-lock (get-pair-key token token-in)))
        ;; return the swap output information
        { 'token: (format "{}" [token])
        , 'amount: amount-out
        }
      )
    )
  )

  (defun create-pair:object{pair}
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      hint:string
      )
    " Create new pair for legs TOKEN0 and TOKEN1. This creates a new \
    \ pair record, a liquidity token named after the canonical pair key \
    \ in the 'tokens' module, and new empty accounts in each leg token. \
    \ If account key value is already taken in leg tokens, transaction \
    \ will fail, which is why HINT exists (which should normally be \"\"), \
    \ to further seed the hash function creating the account id."
    (enforce-contract-unlocked)
    (let* ((key (get-pair-key token0 token1))
           (canon (is-canonical token0 token1))
           (ctoken0:module{fungible-v2} (if canon token0 token1))
           (ctoken1:module{fungible-v2} (if canon token1 token0))
           (a (create-pair-account key hint))
           (t0g (create-reserve-guard key ctoken0))
           (t1g (create-reserve-guard key ctoken1))
           (lpg (create-liquidity-guard key))
           (f (create-fee-account key hint))
           (fg (create-fee-guard key))
           (p { 'leg0: { 'token: ctoken0, 'reserve: 0.0 }
              , 'leg1: { 'token: ctoken1, 'reserve: 0.0 }
              , 'account: a
              , 'guard: (create-null-guard)
              , 'fee-account: f
              , 'fee-guard: fg
              , 'last-k: 0.0
              , 'locked: false
              })
           )
      ;; create the table entry and all the token accounts
      (with-capability (CREATE_PAIR ctoken0 ctoken1 key a)
        (insert pairs key p)
        (ctoken0::create-account a t0g)
        (ctoken1::create-account a t1g)
        (tokens.create-account key a lpg)
        (tokens.create-account key f fg)
        { "key": key
        , "account": a
        }))
    )

  (defun rotate-fee-guard
    ( key:string
      g:guard
    )
    "Internal function for updating the fee-account guard for a given pair."
    (with-capability (GOVERNANCE)
      (let*
        ( (p (read pairs key))
          (fee-account (at 'fee-account p))
          (fee-guard (at 'fee-guard p))
        )
        (with-capability (FEE_ACCOUNT key)
          (tokens.rotate key fee-account g))
        (update pairs key { 'fee-guard: g }))))

  (defun get-pair-by-key:object{pair} (key:string)
    (read pairs key))

  (defun get-pairs:[string] ()
    "Get a list of all the pair keys."
    (keys pairs))

  (defun get-pair-key:string
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Create canonical key for pair."
    (format "{}:{}" (canonicalize tokenA tokenB))
  )

  (defun canonicalize:[module{fungible-v2}]
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Sort the pairs into canonical ordering."
    (if (is-canonical tokenA tokenB) [tokenA tokenB] [tokenB tokenA])
  )

  (defun is-canonical
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Returns whether the tokens are already sorted."
    (< (format "{}" [tokenA]) (format "{}" [tokenB]))
  )

  (defun is-leg0:bool
    ( p:object{pair}
      token:module{fungible-v2}
    )
    "Returns whether the given token is the leg0 of the pair. Note this function does not check if the token is actually one of the two tokens in the pair."
    (let ((token0 (at 'token (at 'leg0 p))))
      (= (format "{}" [token])
         (format "{}" [token0]))) ;; TODO: modref equality
  )

  (defun leg-for:object{leg}
    ( p:object{pair}
      token:module{fungible-v2}
    )
    "Gets the token leg for a given pair. (Does not check if token is actually in the pair)."
    (if (is-leg0 p token)
      (at 'leg0 p)
      (at 'leg1 p))
  )

  (defun reserve-for:decimal
    ( p:object{pair}
      token:module{fungible-v2}
    )
    "Returns the reserve for the specified token."
    (at 'reserve (leg-for p token))
  )

  (defun create-pair-account:string
    ( key:string hint:string )
    "Generates a pair account name."
    (hash (+ hint (+ key (format "{}" [(at 'block-time (chain-data))]))))
  )

  (defun create-fee-account:string
    ( key:string hint:string )
    "Generates a fee account name."
    (create-pair-account key (format "{}-fee" [hint]))
  )

  (defun truncate:decimal (token:module{fungible-v2} amount:decimal)
    "Truncates the given value to the token's precision."
    (floor amount (token::precision))
  )
)

(if (= (read-integer 'upgrade) 0)
    [ ;; deploying from scratch: create all tables
      (create-table contract-lock)
      (create-table pairs)
      (create-table observations-v2)
      (create-table oracles-v2)
      (init (read-msg 'initial-lock))
    ]
    (if (= (read-integer 'upgrade) 1)
        [ ;; upgrade from v1 (devnet deploy) to v2
         "upgrade completed"
        ]
        [(enforce false (format "Invalid upgrade field: {}" [(read-msg 'upgrade)]))]))
