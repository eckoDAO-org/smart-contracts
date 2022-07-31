;; Module responsible for wrapping and unwrapping KDX into other side tokens.
;;
;; These side tokens are reedemable 1:1 for KDX. Usually, the side tokens
;; have restrictions on transfers or similar operations, but are used as
;; auxiliary tokens for programs like staking. For example, when staking
;; 100 KDX, the staking contract will give you back 100 sKDX which cannot
;; be transferred, but is used when exiting the staking program.

(namespace (read-msg 'ns))

(module alchemist GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-keyset 'kaddex-alchemist-admin))

  (defcap OPS ()
    (enforce-keyset 'kaddex-alchemist-ops))

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

  (defcap WRAP
    ( token:module{fungible-v2,supply-control-v1}
      amount:decimal
      sender:string
      receiver:string
    )
    @event
    (enforce-contract-unlocked)
    (enforce-privilege token 'wrap)
  )

  (defcap UNWRAP
    ( token:module{fungible-v2,supply-control-v1}
      amount:decimal
      sender:string
      receiver:string
    )
    @event
    (enforce-contract-unlocked)
    (enforce-privilege token 'unwrap)
  )

  (defun format-token:string (token:module{fungible-v2})
    (format "{}" [token]))

  (defcap RESERVE_ACCESS
    ( token:string )
    true)
  (defcap MINTING
    ( token:string )
    true)
  (defcap BURNING
    ( token:string )
    true)

  (defun enforce-reserve-access (token:module{fungible-v2,supply-control-v1})
    (require-capability (RESERVE_ACCESS (format-token token))))
  (defun enforce-minting (token:module{fungible-v2,supply-control-v1})
    (require-capability (MINTING (format-token token))))
  (defun enforce-burning (token:module{fungible-v2,supply-control-v1})
    (require-capability (BURNING (format-token token))))

  (defun create-reserve-guard (token:module{fungible-v2,supply-control-v1})
    (create-user-guard (enforce-reserve-access token)))
  (defun create-mint-guard (token:module{fungible-v2,supply-control-v1})
    (create-user-guard (enforce-minting token)))
  (defun create-burn-guard (token:module{fungible-v2,supply-control-v1})
    (create-user-guard (enforce-burning token)))

  (defun get-base-token:module{fungible-v2} () kaddex.kdx)

  (defconst PREFIX_LIST_KEY:string "")
  (defschema prefix-list-schema
    tokens:[module{fungible-v2,supply-control-v1}])

  (deftable prefix-list:{prefix-list-schema})

  (defschema prefix
    name:string         ;; same as table key
    token:module{fungible-v2,supply-control-v1}
    account:string      ;; base-token account that holds the backing for prefixed token supply
    held:decimal)

  (deftable prefixes:{prefix})

  (defschema privilege  ;; one row is one prefix-privilege tuple (ex. "wrap to sKDX")
    prefix:string       ;; key from prefixes table
    type:string         ;; a constant like alchemist.WRAP_PRIVILEGE
    guards:[guard])     ;; list of guards, at least one must pass to grant

  (deftable privileges:{privilege})

  (defun get-privilege-key:string
    ( token:module{fungible-v2,supply-control-v1}
      type:string
    )
    (format "{}!{}" [type token])
  )

  (defun or-guard:bool
    ( a:bool
      b:guard )
    (or a (try false (enforce-guard b)))
  )

  (defun enforce-impure:bool
    ( val:bool
      message:string )
    (enforce val message)
  )

  (defun enforce-privilege:bool
    ( token:module{fungible-v2,supply-control-v1}
      type:string )
    (let ((privilege-granted (test-privilege token type)))
      (enforce privilege-granted (format "Could not obtain privilege {} on prefix {}" [type token]))
    )
  )

  (defun test-privilege:bool
    ( token:module{fungible-v2,supply-control-v1}
      type:string
    )
    (with-default-read privileges (get-privilege-key token type)
      { 'guards: [] }
      { 'guards := guards }
      (fold (or-guard) false guards)
    )
  )

  (defun get-holder-account:string
    ( token:module{fungible-v2,supply-control-v1} )
    (at 'account (read prefixes (format "{}" [token])))
  )

  (defun list-prefixes:[module{fungible-v2,supply-control-v1}] ()
    (at 'tokens (read prefix-list PREFIX_LIST_KEY)))

  (defun wrap
    ( amount:decimal
      target-token:module{fungible-v2,supply-control-v1}
      from:string
      to:string
      to-guard:guard
    )
    ;; Enforce wrapping privilege and emit event
    (let ((target-token-name (format-token target-token)))
      (with-capability (WRAP target-token amount from to)
        (with-read prefixes target-token-name
          { 'token := target-module:module{fungible-v2,supply-control-v1}
          , 'held := held
          , 'account := holding-account }
          (enforce (= (format-token target-token) (format-token target-module)) "Token mismatch")
          (let*
            ( (base-token:module{fungible-v2,special-accounts-v1} (get-base-token))
              (start-supply (target-module::total-supply))
              (expected-supply (+ start-supply amount))
              (other-prefixes (filter (!= target-token) (list-prefixes)))
              (other-supplies (map
                (lambda (token:module{supply-control-v1})
                  (let ((token-key (format "{}" [token])))
                    { 'token: token
                    , 'supply: (token::total-supply)
                    , 'held-record: (at 'held (read prefixes token-key))
                    , 'held-balance: (base-token::get-balance (base-token::resolve-special token-key)) })) other-prefixes))
            )
            ;; Halt wrapping if reserves no longer back tokens
            (enforce (= start-supply held)
              (format "{} has broken parity with base token" [target-token]))
            (map (lambda (composite)
              [ (enforce (= (at 'supply composite) (at 'held-record composite))
                  (format "{} has broken parity with base token" [(at 'token composite)]))
                (enforce (>= (at 'held-balance composite) (at 'held-record composite))
                  (format "{} reserves insufficient to back prefix" [(at 'token composite)]))
              ]) other-supplies)

            ;; Take custody of amount base token
            (let ((type (format-token target-token)))
              (with-capability (RESERVE_ACCESS type)
                (base-token::wrap-transfer type from to amount)))

            ;; Mint <amount> target-token
            (with-capability (MINTING (format-token target-token))
              (target-token::mint 'wrap to to-guard amount))

            (let
              ( (after-supply (target-token::total-supply))
              )
              ;; Enforce mass conservation
              (enforce (= expected-supply after-supply)
                (format "{} mass not conserved ({} in, expected total supply to reach {} but was {})"
                  [target-token amount expected-supply after-supply]))
              (map (lambda (composite)
                (bind composite { 'token := token:module{fungible-v2,supply-control-v1} }
                  [ (enforce-impure (= (token::total-supply) (at 'held-record composite))
                      (format "{} has broken parity with base token" [token]))
                    (enforce-impure
                      (>= (base-token::get-balance (base-token::resolve-special (format "{}" [token])))
                          (at 'held-record composite))
                      (format "{} reserves insufficient to back prefix" [(at 'token composite)]))
                  ])) other-supplies)

              ;; Record updated backing amount
              (update prefixes target-token-name { 'held: (+ held amount) })
              (format "Wrapped {} base token from account \"{}\" to {} {} in account {}"
                [amount from amount target-token to])
            )
          )
        )
      )
    )
  )

  (defun unwrap
    ( amount:decimal
      source-token:module{fungible-v2,supply-control-v1}
      from:string
      to:string
      to-guard:guard)
    ;; Enforce unwrapping privilege and emit event
    (let ((source-token-name (format-token source-token)))
      (with-capability (UNWRAP source-token amount from to)
        (with-read prefixes source-token-name
          { 'token := source-module:module{fungible-v2,supply-control-v1}
          , 'held := held
          , 'account := holding-account }
          (enforce (= (format-token source-module) (format-token source-token)) "Token mismatch")
          (let*
            ( (base-token:module{fungible-v2,special-accounts-v1} (get-base-token))
              (start-supply (source-module::total-supply))
              (expected-supply (- start-supply amount))
              (other-prefixes (filter (!= source-token) (list-prefixes)))
              (other-supplies (map
                (lambda (token:module{supply-control-v1})
                  (let ((token-key (format "{}" [token])))
                    { 'token: token
                    , 'supply: (token::total-supply)
                    , 'held-record: (at 'held (read prefixes token-key))
                    , 'held-balance: (base-token::get-balance (base-token::resolve-special token-key)) })) other-prefixes))
            )
            ;; Halt wrapping if reserves no longer back tokens
            (enforce (= start-supply held) (format "{} has broken parity with base token" [source-token]))
            (map (lambda (composite)
              [ (enforce (= (at 'supply composite) (at 'held-record composite))
                  (format "{} has broken parity with base token" [(at 'token composite)]))
                (enforce (>= (at 'held-balance composite) (at 'held-record composite))
                  (format "{} reserves insufficient to back prefix" [(at 'token composite)]))
              ]) other-supplies)

            ;; Burn <amount> source-token
            (with-capability (BURNING (format-token source-token))
              (source-token::burn 'unwrap from amount))

            ;; Transfer custody of <amount> base token
            (let ((type (format-token source-token)))
              (with-capability (RESERVE_ACCESS type)
                (base-token::unwrap-transfer type from to to-guard amount)))

            (let
              ( (after-supply (source-token::total-supply))
              )
              ;; Enforce mass conservation
              (enforce-impure (= expected-supply after-supply)
                (format "{} mass not conserved ({} out, expected total supply to reach {} but was {})"
                  [source-token amount expected-supply after-supply]))
              (map (lambda (composite)
                (bind composite { 'token := token:module{fungible-v2,supply-control-v1} }
                  [ (enforce-impure (= (token::total-supply) (at 'held-record composite))
                      (format "{} has broken parity with base token" [token]))
                    (enforce-impure
                      (>= (base-token::get-balance (base-token::resolve-special (format "{}" [token])))
                          (at 'held-record composite))
                      (format "{} reserves insufficient to back prefix" [(at 'token composite)]))
                  ])) other-supplies)

              ;; Record updated backing amount
              (update prefixes source-token-name { 'held: (- held amount) })
              (format "Unwrapped {} {} token from account \"{}\" to {} base token in account {}"
                [amount source-token from amount to])
            )
          )
        )
      )
    )
  )

  (defun register-prefix
    ( token:module{fungible-v2,supply-control-v1}
      hint:string
    )
    (with-capability (OPS)
      (let*
        ( (name (format "{}" [token]))
          (holder-account-name (hash (format "holder_{}_{}" [name hint])))
          (base-token:module{fungible-v2,special-accounts-v1} (get-base-token))
        )
        (base-token::create-account holder-account-name (create-reserve-guard token))
        (base-token::assign-special name holder-account-name)
        (insert prefixes name
          { 'name: name
          , 'token: token
          , 'account: holder-account-name
          , 'held: 0.0 })
        (with-read prefix-list PREFIX_LIST_KEY { 'tokens := previous-tokens }
          (update prefix-list PREFIX_LIST_KEY { 'tokens: (+ previous-tokens [token]) }))
      )
    )
  )

  (defun grant-privilege
    ( token:module{fungible-v2,supply-control-v1}
      privilege:string
      guard:guard
    )
    (with-capability (OPS)
      (let*
        ( (prefix (format "{}" [token]))
          (privilege-key (get-privilege-key token privilege))
        )
        (with-default-read privileges privilege-key
          { 'prefix: prefix
          , 'type: privilege
          , 'guards: [] }
          { 'prefix := prefix-read
          , 'type := privilege-read
          , 'guards := existing-guards }
          (enforce (= prefix-read prefix) "Prefix mismatch")
          (enforce (= privilege-read privilege) "Privilege mismatch")
          (write privileges privilege-key
            { 'prefix: prefix
            , 'type: privilege
            , 'guards: (+ existing-guards [guard]) })
        )
      )
    )
  )
)


(if (= (read-integer 'upgrade) 0)
    [ ;; deploying from scratch: create all tables
      (create-table contract-lock)
      (insert contract-lock CONTRACT_LOCK_KEY {'lock: true})
      (create-table prefixes)
      (create-table privileges)
      (create-table prefix-list)
      (insert prefix-list PREFIX_LIST_KEY { 'tokens: [] })
    ]
    (if (= (read-integer 'upgrade) 1)
        [ ;; upgrade from v1 (devnet deploy) to v2 -- no schema changes
          "upgrade complete"
        ]
        [(enforce false (format "Invalid upgrade field: {}" [(read-msg 'upgrade)]))]))
