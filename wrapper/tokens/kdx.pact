;; Module code for the KDX token.
;;
;; Simple fungible-v2 module that keeps track of mints and total supply.
;; Each chain has its own mint-cap, which is used to keep track of
;; the tokenomics distribution.
;;
;; Uses a straightforward privilege system to delegate MINT/BURN permissions.

(namespace (read-msg 'ns))

(module kdx GOVERNANCE
  @model
    [ (defproperty conserves-mass
        (= (column-delta token-table 'balance) 0.0))

      (defproperty valid-account (account:string)
        (and
          (>= (length account) 3)
          (<= (length account) 256)))
    ]

  (implements special-accounts-v2)
  (implements supply-control-v1)
  (implements fungible-v2)
  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema token-schema
    @doc "The KDX contract token schema"
    @model [ (invariant (>= balance 0.0)) ]

    balance:decimal
    guard:guard)

  (deftable token-table:{token-schema})

  (defschema token-supply
      total-burned:decimal
      total-minted:decimal)

  (deftable supply-table:{token-supply})

  (defschema privilege
    guard:guard)

  (deftable privileges:{privilege})

  (defschema special-account
    account:string)

  (deftable special-accounts:{special-account})

  (defschema chain-balance
    chain:string
    balance:decimal)

  (deftable chain-balances:{chain-balance})

  (defschema mint-cap
    maximum:decimal)

  (deftable mint-cap-table:{mint-cap})

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

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce-keyset 'kdx-admin-keyset))

  (defcap OPS ()
    (enforce-keyset 'kaddex-ops-keyset))

  (defcap BALANCE_UPDATE ()
    "Internal capability for updating chain balance data"
    true)

  (defcap BURN
    ( sender:string
      amount:decimal
    )
    "Capability for privileged intra-ecosystem burns"
    (enforce-contract-unlocked)
    (enforce-privilege BURN_PRIVILEGE)
    (compose-capability (DEBIT sender))
  )

  (defcap MINT
    ( receiver:string
      amount:decimal
    )
    (enforce-contract-unlocked)
    (enforce-privilege MINT_PRIVILEGE)
    (compose-capability (CREDIT receiver))
  )

  (defcap DEBIT (sender:string)
    "Capability for managing debiting operations"
    (enforce-contract-unlocked)
    (enforce-guard (at 'guard (read token-table sender)))
    (enforce (!= sender "") "valid sender"))

  (defcap CREDIT (receiver:string)
    "Capability for managing crediting operations"
    (enforce-contract-unlocked)
    (enforce (!= receiver "") "valid receiver"))

  (defcap ROTATE (account:string)
    @doc "Autonomously managed capability for guard rotation"
    @managed
    (enforce-contract-unlocked)
  )

  (defcap WRAP:bool
    ( type:module{fungible-v2,supply-control-v1}
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount WRAP-mgr
    (enforce-contract-unlocked)
    (enforce-unit amount)
    (enforce (> amount 0.0) "amount must be positive")
    (let ((resolved-receiver (resolve-special type)))
      (enforce (!= sender resolved-receiver) "sender and receiver same")
      (compose-capability (DEBIT sender))
      (compose-capability (CREDIT resolved-receiver)))
    )

  (defun WRAP-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "WRAP exceeded for balance {}" [managed]))
      newbal)
  )

  (defcap UNWRAP:bool
    ( type:module{fungible-v2,supply-control-v1}
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount UNWRAP-mgr
    (enforce-contract-unlocked)
    (enforce-unit amount)
    (enforce (> amount 0.0) "amount must be positive")
    (let ((resolved-sender (resolve-special type)))
      (enforce (!= receiver resolved-sender) "sender and receiver same")
      (compose-capability (DEBIT resolved-sender))
      (compose-capability (CREDIT receiver))))

  (defun UNWRAP-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "UNWRAP exceeded for balance {}" [managed]))
      newbal)
  )

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @doc "Autonomously managed capability for token transfers"
    @managed amount TRANSFER-mgr
    (enforce-contract-unlocked)
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst TOKEN_PURPOSES
    [{ 'purpose: 'team,            'percentage-cap: 0.05 }
     { 'purpose: 'token-sales,     'percentage-cap: 0.3 }
     { 'purpose: 'dao-treasury,    'percentage-cap: 0.25 }
     { 'purpose: 'network-rewards, 'percentage-cap: 0.4 }
     { 'purpose: 'burn,            'percentage-cap: 0.0 }
     ] ;; the percentage-cap says how much % of the MAX_SUPPLY can be minted for this purpose
    )

  (defconst MAX_SUPPLY:decimal 1000000000.0) ;; 1 billion KDX max supply

  (defconst COIN_CHARSET CHARSET_LATIN1
    "The default KDX contract character set")

  (defconst MINIMUM_PRECISION 12
    "Minimum allowed precision for KDX transactions")

  (defconst MINIMUM_ACCOUNT_LENGTH 3
    "Minimum account length admissible for KDX accounts")

  (defconst MAXIMUM_ACCOUNT_LENGTH 256
    "Maximum account name length admissible for KDX accounts")

  ;; validate the chain ID when doing a crosschain transfer to prevent funds stuck in limbo
  (defconst VALID_CHAINS ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"])

  (defconst ISSUANCE_ACCOUNT 'kdx-bank)

  (defconst BURN_PRIVILEGE 'burn)

  (defconst MINT_PRIVILEGE 'mint)

  (defconst MINT_CAP_KEY 'cap)

  ; --------------------------------------------------------------------------
  ; Utilities

  (defun resolve-special:string (type:module{fungible-v2,supply-control-v1})
    (with-read special-accounts (format "{}" [type])
      { 'account := account }
        account))

  (defun assign-special:string (type:module{fungible-v2,supply-control-v1} account:string)
    (with-capability (GOVERNANCE)
      (write special-accounts (format "{}" [type]) { 'account: account })))

  (defun get-purpose-list:[string] ()
    (map (lambda (o) (at 'purpose o)) TOKEN_PURPOSES))

  (defun enforce-valid-purpose (purpose:string)
    (enforce (contains purpose (get-purpose-list)) (format "Invalid token purpose {}" [purpose]))
  )

  (defun get-purpose-max-cap (purpose:string)
    (enforce-valid-purpose purpose)
    (let ( (p (filter (lambda (x) (= purpose (at 'purpose x))) TOKEN_PURPOSES)) )
      (enforce (= (length p) 1) "sanity check: purpose not found or multiple purposes found")
      (* MAX_SUPPLY (at 'percentage-cap (at 0 p)))
    )
  )

  (defun get-mint-cap () (at 'maximum (read mint-cap-table MINT_CAP_KEY)))

  (defun update-mint-cap (new-cap:decimal)
    (enforce (>= new-cap 0.0) "new cap must be nonnegative")
    (with-capability (GOVERNANCE)
      (write mint-cap-table MINT_CAP_KEY { 'maximum: new-cap }))
  )

  (defun update-supply (purpose:string delta:decimal account:string action:string)
    (enforce-valid-purpose purpose)
    (enforce (or (= action 'burn) (= action 'mint)) (format "Invalid supply action {}" [action]))
    (validate-account account)
    (enforce-unit delta)
    (if (= action 'burn)
        (require-capability (DEBIT account))
        (require-capability (CREDIT account)))
    (let
      ( (supply (read supply-table purpose))
        (chain (at 'chain-id (chain-data)))
      )
      (if (= action 'burn)
          (with-capability (BALANCE_UPDATE)
            (update-chain-balance chain (- 0.0 delta))
            (update supply-table purpose { 'total-burned: (+ delta (at 'total-burned supply)) }))
          (with-capability (BALANCE_UPDATE)
            (let
              ((minted (total-minted))
               (available (get-mint-cap)))
              (enforce (< (+ minted delta) available)
                (format "Already minted {}, minting {} exceeds cap {}"
                  [ minted delta available ])))
            (update-chain-balance chain delta)
            (update supply-table purpose { 'total-minted: (+ delta (at 'total-minted supply)) }))
      )
    )
  )

  (defun update-chain-balance (chain:string delta:decimal)
    (require-capability (BALANCE_UPDATE))
    (update chain-balances chain { 'balance: (+ delta (get-chain-balance chain)) })
  )

  (defun get-chain-balance:decimal (chain:string)
    (at 'balance (read chain-balances chain)))

  (defun get-supply (purpose:string)
    (enforce-valid-purpose purpose)
    (let ((supply (read supply-table purpose)))
      (- (at 'total-minted supply) (at 'total-burned supply)))
  )

  (defun get-raw-supply (purpose:string)
    (enforce-valid-purpose purpose)
    (read supply-table purpose)
  )

  (defun total-minted:decimal ()
    (fold (+) 0.0 (map (lambda (purpose) (at 'total-minted (read supply-table purpose))) (get-purpose-list)))
  )

  (defun total-supply:decimal ()
    (fold (+) 0.0 (map (get-supply) (get-purpose-list)))
  )

  (defun enforce-privilege (privilege:string)
    (enforce-guard (at 'guard (read privileges privilege ['guard])))
  )

  (defun assign-privilege
    ( privilege:string
      guard:guard
    )
    (require-capability (GOVERNANCE))
    (write privileges privilege { 'guard: guard })
  )

  (defun enforce-unit:bool (amount:decimal)
    @doc "Enforce minimum precision allowed for KDX transactions"

    (enforce
      (= (floor amount MINIMUM_PRECISION)
         amount)
      (format "Amount violates minimum precision: {}" [amount]))
    )

  (defun validate-account (account:string)
    @doc "Enforce that an account name conforms to the KDX contract \
         \minimum and maximum length requirements, as well as the    \
         \latin-1 character set."

    (enforce
      (is-charset COIN_CHARSET account)
      (format
        "Account does not conform to the KDX contract charset: {}"
        [account]))

    (let ((account-length (length account)))

      (enforce
        (>= account-length MINIMUM_ACCOUNT_LENGTH)
        (format
          "Account name does not conform to the min length requirement: {}"
          [account]))

      (enforce
        (<= account-length MAXIMUM_ACCOUNT_LENGTH)
        (format
          "Account name does not conform to the max length requirement: {}"
          [account]))
      )
  )

  (defun enforce-valid-chain (chain-id:string)
    @doc "Enforce that the target chain id is valid."
    (enforce (!= "" chain-id) "Empty chain ID")
    (enforce (!= (at 'chain-id (chain-data)) chain-id) "Cannot run cross-chain transfers to the same chain")
    (enforce (contains chain-id VALID_CHAINS) (format "Chain ID {} is invalid or unknown" [chain-id]))
  )

  (defun create-account:string (account:string guard:guard)
    @model [ (property (valid-account account)) ]

    (validate-account account)
    (enforce-reserved account guard)

    (insert token-table account
      { "balance" : 0.0
      , "guard"   : guard
      })
    )

  (defun get-balance:decimal (account:string)
    (with-read token-table account
      { "balance" := balance }
      balance
      )
    )

  (defun details:object{fungible-v2.account-details}
    ( account:string )
    (with-read token-table account
      { "balance" := bal
      , "guard" := g }
      { "account" : account
      , "balance" : bal
      , "guard": g })
    )

  (defun rotate:string (account:string new-guard:guard)
    (with-capability (ROTATE account)
      (with-read token-table account
        { "guard" := old-guard }

        (enforce-guard old-guard)

        (update token-table account
          { "guard" : new-guard }
          )))
    )


  (defun precision:integer
    ()
    MINIMUM_PRECISION)

  (defun burn:decimal (purpose:string account:string amount:decimal)
    (validate-account account)
    (enforce-unit amount)

    (enforce-privilege BURN_PRIVILEGE)

    (with-capability (BURN account amount)
      (debit account amount)
      (update-supply purpose amount account 'burn)
      amount
    )
  )

  (defun mint:decimal (purpose:string account:string guard:guard amount:decimal)
    (validate-account account)
    (enforce-unit amount)

    (enforce-privilege MINT_PRIVILEGE)

    (with-capability (MINT account amount)
      (credit account guard amount)
      (update-supply purpose amount account 'mint)
      amount
    )
  )

  (defun wrap-transfer:string (type:module{fungible-v2,supply-control-v1} sender:string receiver:string amount:decimal)
    @model [ (property conserves-mass)
             (property (> amount 0.0))
             (property (valid-account sender)) ]
    (validate-account sender)
    (enforce (> amount 0.0) "amount must be positive")
    (enforce-unit amount)

    (let ((holder (resolve-special type)))
      (enforce (!= holder sender) "cannot use special account to wrap")
      (with-capability (WRAP type sender receiver amount)
        (debit sender amount)
        (with-read token-table holder
          { 'guard := g }
            (credit holder g amount)))))

  (defun unwrap-transfer:string (type:module{fungible-v2,supply-control-v1} sender:string receiver:string receiver-guard:guard amount:decimal)
    @model [ (property conserves-mass)
             (property (> amount 0.0))
             (property (valid-account receiver)) ]
    (validate-account receiver)
    (enforce (> amount 0.0) "amount must be positive")
    (enforce-unit amount)

    (let ((holder (resolve-special type)))
      (enforce (!= holder receiver) "cannot use special account to unwrap")
      (with-capability (UNWRAP type sender receiver amount)
        (debit holder amount)
        (credit receiver receiver-guard amount))))

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @model [ (property conserves-mass)
             (property (> amount 0.0))
             (property (valid-account sender))
             (property (valid-account receiver))
             (property (!= sender receiver)) ]

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (validate-account sender)
    (validate-account receiver)

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (enforce-unit amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (with-read token-table receiver
        { "guard" := g }

        (credit receiver g amount))
      )
    )

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )

    @model [ (property conserves-mass) ]

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (validate-account sender)
    (validate-account receiver)

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (enforce-unit amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount))
    )

  (defun debit:string (account:string amount:decimal)
    @doc "Debit AMOUNT from ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]

    (validate-account account)

    (enforce (> amount 0.0)
      "debit amount must be positive")

    (enforce-unit amount)

    (require-capability (DEBIT account))
    (with-read token-table account
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update token-table account
        { "balance" : (- balance amount) }
        ))
    )


  (defun credit:string (account:string guard:guard amount:decimal)
    @doc "Credit AMOUNT to ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]

    (validate-account account)

    (enforce (> amount 0.0) "credit amount must be positive")
    (enforce-unit amount)

    (require-capability (CREDIT account))
    (with-default-read token-table account
      { "balance" : -1.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")

      (let ((is-new
             (if (= balance -1.0)
                 (enforce-reserved account guard)
                 false)))

        (write token-table account
          { "balance" : (if is-new amount (+ balance amount))
          , "guard"   : retg
          }))
      ))

  (defun check-reserved:string (account:string)
    " Checks ACCOUNT for reserved name and returns type if \
    \ found or empty string. Reserved names start with a \
    \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
    (let ((pfx (take 2 account)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-reserved:bool (account:string guard:guard)
    @doc "Enforce reserved account name protocols."
    (if (validate-principal guard account)
      true
      (let ((r (check-reserved account)))
        (if (= r "")
          true
          (if (= r "k")
            (enforce false "Single-key account protocol violation")
            (enforce false
              (format "Reserved protocol guard violation: {}" [r])))))))

  (defschema crosschain-schema
    @doc "Schema for yielded value in cross-chain transfers"
    receiver:string
    receiver-guard:guard
    amount:decimal)

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )

    @model [ (property (> amount 0.0))
             (property (valid-account sender))
             (property (valid-account receiver))
           ]

    (step
      (with-capability (DEBIT sender)

        (validate-account sender)
        (validate-account receiver)

        (enforce-valid-chain target-chain)

        (enforce (> amount 0.0)
          "transfer quantity must be positive")

        (enforce-unit amount)

        ;; step 1 - debit delete-account on current chain
        (debit sender amount)

        (emit-event (TRANSFER sender "" amount))

        (with-capability (BALANCE_UPDATE)
          (let*
            ((chain-id (at 'chain-id (chain-data)))
             ;; Update local copy of local chain balance
             (ignore (update-chain-balance chain-id (- 0.0 amount)))
             ;; Update local copy of target chain balance (assumes crosschain success)
             (ignore_ (update-chain-balance target-chain amount))
             (crosschain-details:object{crosschain-schema}
              { "receiver": receiver
              , "receiver-guard": receiver-guard
              , "amount": amount
              , "source-chain": chain-id
              , "source-chain-balance": (get-chain-balance chain-id) }))
            (yield crosschain-details target-chain)
          )
        )))

    (step
      (resume
        { "receiver" := receiver
        , "receiver-guard" := receiver-guard
        , "amount" := amount
        , "source-chain" := source-chain
        , "source-chain-balance" := source-chain-balance
        }
        (emit-event (TRANSFER "" receiver amount))
        ;; Update local copy of source chain balance
        (update chain-balances source-chain { 'balance: source-chain-balance })
        ;; Update local copy of local chain balance
        (with-capability (BALANCE_UPDATE)
          (update-chain-balance (at 'chain-id (chain-data)) amount))
        ;; step 2 - credit create account on target chain
        (with-capability (CREDIT receiver)
          (credit receiver receiver-guard amount))
        ))
    )

    (defun init ()
      (insert contract-lock CONTRACT_LOCK_KEY {'lock: false})
      (with-capability (GOVERNANCE)
        (insert token-table ISSUANCE_ACCOUNT
          { 'balance: 0.0
          , 'guard: (read-keyset 'kdx-admin-keyset) })
        (update-mint-cap 0.0)
        (let
          ( (create-supply-table (lambda (purpose)
                                   (insert supply-table purpose
                                           { 'total-minted: 0.0, 'total-burned: 0.0 })))
            (create-chain-record (lambda (chain-id)
              (insert chain-balances chain-id { 'chain: chain-id, 'balance: 0.0 })))
          )
          (map (create-supply-table) (get-purpose-list))
          (map (create-chain-record) VALID_CHAINS)
        )
        (assign-privilege BURN_PRIVILEGE (read-keyset 'kdx-admin-keyset))
        (assign-privilege MINT_PRIVILEGE (read-keyset 'kdx-admin-keyset))
      )
    )
)

(if (= (read-integer 'upgrade) 0)
    [ ;; deploying from scratch: create all tables
      (create-table contract-lock)
      (create-table token-table)
      (create-table privileges)
      (create-table supply-table)
      (create-table special-accounts)
      (create-table chain-balances)
      (create-table mint-cap-table)
      (init)
    ]
    (if (= (read-integer 'upgrade) 1)
        [ ;; upgrade from v1 (devnet deploy) to v2 -- no schema changes
          "upgrade complete"
        ]
        [(enforce false (format "Invalid upgrade field: {}" [(read-msg 'upgrade)]))]))
