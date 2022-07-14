(namespace (read-msg 'ns))
(module priv-sale GOVERNANCE

  (use coin)
  (use util.guards)

  (defschema reservation
    account:string
    guard:guard
    time:time
    amount-kda:decimal
    amount-kdx:decimal
    status:string)

  (deftable reservations:{reservation})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-admin)))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-ops)))

  (defcap RESERVE
    ( account:string
      amount-kda:decimal)
    "Reserve event for kdx reservation"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-guard (at-after-date START_TIME))
  )

  (defconst KDX_BANK:string 'kdx-bank)

  (defconst KDX_SALE_SUPPLY:decimal 10000000.0)

  (defconst KDX_MAX_RATIO:decimal 10.0)

  (defconst KDA_SALE_MIN:decimal 1000000.0)

  (defconst STATUS_REQUESTED:string 'requested)

  (defconst STATUS_APPROVED:string 'approved)

  (defconst STATUS_REJECTED:string 'rejected)

  (defconst START_TIME:time (time "2021-06-10T23:59:00Z"))

  (defconst END_TIME:time (time "2021-06-24T23:59:00Z"))

  (defun kdx-bank-guard () (create-module-guard "kaddex-admin"))

  (defun init ()
    (coin.create-account KDX_BANK (kdx-bank-guard))
  )

  (defun reserve:string (account:string amount-kda:decimal)
    (with-capability (RESERVE account amount-kda)
      (coin.transfer account KDX_BANK amount-kda)
      (let
        ( (tx-id (hash {"account": account, "amount": amount-kda, "salt": (at "block-time" (chain-data))}))
          (g (at 'guard (coin.details account)))
        )
        (insert reservations (format "{}-{}" [account, tx-id])
          { "account"    : account
          , "amount-kda" : amount-kda
          , "amount-kdx" : 0.0
          , "time"       : (at "block-time" (chain-data))
          , "guard"      : g
          , "status"     : STATUS_REQUESTED
          })
        (format "{} reserved KDX with {} KDA" [account, amount-kda])
      )
    )
  )

  (defun approve:string (reservation-id:string)
    (with-capability (OPS)
      (with-read reservations reservation-id
        { "status"     := status }
        (enforce (= status STATUS_REQUESTED) "request is not open")
        (update reservations reservation-id
          { "status" : STATUS_APPROVED })
        (format "request {} approved" [reservation-id])
      )
    )
  )

  (defun reject:string (reservation-id:string)
    (with-capability (OPS)
      (with-read reservations reservation-id
        { "status"     := status
        , "amount-kda" := amount-kda
        , "account"    := account }
        (enforce (= status STATUS_REQUESTED) "request is not open")
        (update reservations reservation-id
          { "status" : STATUS_REJECTED })
        (install-capability (coin.TRANSFER KDX_BANK account amount-kda))
        (coin.transfer KDX_BANK account amount-kda)
        (format "request {} rejected" [reservation-id])
      )
    )
  )


  (defun approve-helper:string (reservation-id:string)
    (require-capability (OPS))
    (with-read reservations reservation-id
      { "status"     := status }
      (if (= status STATUS_REQUESTED)
        (update reservations reservation-id
          { "status" : STATUS_APPROVED })
        "skipping case"
      )
    )
  )

  (defun approve-all:string ()
    (with-capability (OPS)
      (map (approve-helper) (get-tx-ids))
    )
  )

  (defun update-kdx-amount (kdx-ratio:decimal reservation-id:string)
    (require-capability (OPS))
    (with-read reservations reservation-id
      { "status"     := status
      , "amount-kda" := amount-kda
      , "account"    := account }
      (enforce (!= status STATUS_REQUESTED) "one or more requests are still open")
      (if (= status STATUS_APPROVED)
        (update reservations reservation-id
          { "amount-kdx" : (* amount-kda kdx-ratio) })
        "skipping case"
      )
    )
  )

  (defun end-sale ()
    (with-capability (OPS)
      (let*
        (
          (approved (select reservations (where 'status (= STATUS_APPROVED))))
          (total-kda (fold (+) 0.0 (map (at "amount-kda") approved)))
          (ratio (if (> total-kda KDA_SALE_MIN) (/ KDX_SALE_SUPPLY total-kda) KDX_MAX_RATIO))
        )
        (map (update-kdx-amount ratio) (get-tx-ids))
      )
    )
  )

  (defun read-reservations (account:string)
    (select reservations (where 'account (= account)))
  )

  (defun read-all-reservations ()
    (map (read reservations) (get-tx-ids))
  )

  (defun get-tx-ids ()
    (keys reservations)
  )

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table reservations)
    (init) ])
