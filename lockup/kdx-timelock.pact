(namespace (read-msg 'ns))
(module time-lock GOVERNANCE

  (use coin)
  (use util.guards)

  (defschema return-amount
    kda:decimal
    kdx:decimal)

  (defschema lockup-option
    length:decimal ; seconds
    kda-return:decimal  ; fraction (e.g. 0.04 for 4% of original kda rewarded back)
    kdx-return:decimal) ; fraction (e.g. 0.21 for 21% of original kdx rewarded back)

  (deftable lockup-options:{lockup-option})

  (defschema lockup
    account:string
    reservation-guard:guard
    payout-account:string
    payout-guard:guard
    request-time:time
    lockup-length:string
    lockup-start-time:time
    lockup-end-time:time
    kdx-locked:decimal
    kdx-returned:decimal
    kdx-total-return:decimal
    kda-returned:decimal
    kda-total-return:decimal
    initial-kdx-withdrawn:bool
    status:string)

  (deftable lockups:{lockup})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-timelock-admin)))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-timelock-ops)))

  (defcap LOCK
    ( reservation-account:string
      lockup-length:decimal)
    "Lockup KDX rights event"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-guard (at-after-date START_TIME))
    (let ((reservation-guard (at 'guard (coin.details reservation-account))))
      (enforce-guard reservation-guard))
  )

  (defcap CLAIM (reservation-account:string)
    (let ((reservation-guard (at 'reservation-guard (read lockups reservation-account))))
      (enforce-guard reservation-guard))
    ; QUESTION: enforce receiver account guard or reservation guard at this point?
  )

  (defcap AGGREGATOR_NOTIFY () true)

  (defconst KDX_BANK:string 'kdx-lockup-reward-bank)

  (defconst STATUS_REQUESTED:string 'requested)

  (defconst STATUS_APPROVED:string 'approved)

  (defconst STATUS_REJECTED:string 'rejected)

  (defconst LOCKUP_INTERVAL_LENGTH:decimal (days 90))

  (defconst START_TIME:time (time "2022-07-25T05:00:00Z"))

  (defconst END_TIME:time (time "2022-07-31T08:00:00Z"))

  (defconst KDX_RATIO:decimal 50.0)

  (defconst ASAP_LOCKUP_LENGTH:string 'asap)

  (defun enforce-aggregator () (require-capability (AGGREGATOR_NOTIFY)))
  (defun aggregator-guard:guard () (create-user-guard (enforce-aggregator)))

  (defun kdx-bank-guard:guard () (create-module-guard "kaddex-admin"))

  (defun init ()
    (coin.create-account KDX_BANK (kdx-bank-guard))
    (kaddex.kdx.create-account KDX_BANK (kdx-bank-guard))
  )

  (defun lock:string
    ( reservation-account:string
      payout-account:string
      payout-guard:guard
      lockup-time:string)
    (let*
      ( (lockup-length (read lockup-options lockup-time))
        (lockup-seconds (at 'length lockup-length))
        (kda-return-fraction (at 'kda-return lockup-length))
        (kdx-return-fraction (at 'kdx-return lockup-length))
        (original-reservations (filter
          (compose (at 'status) (!= kdx.priv-sale.STATUS_REJECTED))
          (kdx.priv-sale.read-reservations reservation-account)))
        (kda-paid (fold (+) 0.0 (map (at 'amount-kda) original-reservations)))
        (kdx-allocated (* kda-paid KDX_RATIO))
        (kda-return (floor (* kda-paid kda-return-fraction) (coin.precision)))
        (kdx-return (floor (* kdx-allocated kdx-return-fraction) (kaddex.kdx.precision)))
        (reservation-guard (at 'guard (coin.details reservation-account)))
        (current-time (at 'block-time (chain-data)))
      )
      (enforce (> kdx-allocated 0.0) "You must have a nonnegative KDX reservation.")
      (with-capability (LOCK reservation-account lockup-seconds)
        (insert lockups reservation-account
          { 'account: reservation-account
          , 'reservation-guard: reservation-guard
          , 'payout-account: payout-account
          , 'payout-guard: payout-guard
          , 'request-time: (at 'block-time (chain-data))
          , 'lockup-length: lockup-time
          , 'lockup-start-time: current-time
          , 'lockup-end-time: (add-time current-time lockup-seconds)
          , 'status: STATUS_REQUESTED
          , 'kdx-locked: kdx-allocated
          , 'kda-returned: 0.0
          , 'kda-total-return: kda-return
          , 'kdx-returned: 0.0
          , 'kdx-total-return: kdx-return
          , 'initial-kdx-withdrawn: false })
        (format "{} requested lockup of {} KDX, with {} KDA and {} KDX returned to {} over {}."
          [reservation-account, kdx-allocated, kda-return, kdx-return, payout-account, lockup-time])
      )
    )
  )

  (defun claim-return:object{return-amount} (reservation-account:string)
    (let*
      ( (lockup-record (read lockups reservation-account))
        (available:object{return-amount} (available-return reservation-account))
        (kda-available (at 'kda available))
        (kdx-available (at 'kdx available))
      )
      (enforce (> kda-available 0.0) "Insufficient KDA return.")
      (enforce (> kdx-available 0.0) "Insufficient KDX return.")
      (bind lockup-record
        { 'kda-returned := kda-returned
        , 'kdx-returned := kdx-returned
        , 'payout-account := payout-account
        , 'payout-guard := payout-guard }
        (with-capability (CLAIM reservation-account)
          ;; Payout KDA
          (install-capability (coin.TRANSFER KDX_BANK payout-account kda-available))
          (coin.transfer-create KDX_BANK payout-account payout-guard kda-available)
          ;; Payout KDX
          (install-capability (kaddex.kdx.TRANSFER KDX_BANK payout-account kdx-available))
          (kaddex.kdx.transfer-create KDX_BANK payout-account payout-guard kdx-available)
          (update lockups reservation-account
            { 'kda-returned: (+ kda-returned kda-available)
            , 'kdx-returned: (+ kdx-returned kdx-available) })
          { 'kda: kda-available, 'kdx: kdx-available }
        )
      )
    )
  )

  (defun available-return:object{return-amount} (reservation-account:string)
    (let*
      ( (lockup-record (read lockups reservation-account))
        (return-progress:object{return-amount}
          (calculate-return reservation-account (at 'block-time (chain-data))))
        (kda-unlocked (at 'kda return-progress))
        (kdx-unlocked (at 'kdx return-progress))
        (kda-withdrawn (at 'kda-returned lockup-record))
        (kdx-withdrawn (at 'kdx-returned lockup-record))
      )
      { 'kda: (- kda-unlocked kda-withdrawn)
      , 'kdx: (- kdx-unlocked kdx-withdrawn) }
    )
  )

  (defun calculate-return:object{return-amount}
    ( reservation-account:string
      when:time)
    (with-read lockups reservation-account
      { 'status := status
      , 'kda-total-return := kda-total-return
      , 'kdx-total-return := kdx-total-return
      , 'lockup-start-time := lockup-start
      , 'lockup-end-time := lockup-end }
      (if (!= status STATUS_APPROVED) { 'kda: 0.0, 'kdx: 0.0 }
      (if (< when lockup-start) { 'kda: 0.0, 'kdx: 0.0 }
        (if (> when lockup-end) { 'kda: kda-total-return, 'kdx: kdx-total-return }
          (let*
            ( (lockup-length:decimal (diff-time lockup-end lockup-start))
              (lockup-progress-seconds:decimal (diff-time when lockup-start))
              (lockup-intervals-total:decimal (/ lockup-length LOCKUP_INTERVAL_LENGTH))
              (lockup-intervals-passed:decimal (floor (/ lockup-progress-seconds LOCKUP_INTERVAL_LENGTH) 0))
              (lockup-progress-fraction:decimal (/ lockup-intervals-passed lockup-intervals-total))
            )
            { 'kda: (floor (* lockup-progress-fraction kda-total-return) (coin.precision))
            , 'kdx: (floor (* lockup-progress-fraction kdx-total-return) (kaddex.kdx.precision)) }
          )
        )
      ))
    )
  )

  ;;Utilities

  (defun is-kdx-locked:bool (reservation-account:string)
    (with-default-read lockups reservation-account
      { 'status: STATUS_REJECTED
      , 'lockup-end-time: (parse-time "%s" "0") }
      { 'status := status
      , 'lockup-end-time := lockup-end-time }
      (if (!= status STATUS_APPROVED)
        false
        (< (at 'block-time (chain-data)) lockup-end-time))
    )
  )

  (defun approve:string (lockup-id:string)
    (with-capability (OPS)
      (with-read lockups lockup-id
        { "status"     := status }
        (enforce (= status STATUS_REQUESTED) "request is not open")
        (update lockups lockup-id
          { "status" : STATUS_APPROVED })
        (format "request {} approved" [lockup-id])
      )
    )
  )

  (defun reject:string (lockup-id:string)
    (with-capability (OPS)
      (with-read lockups lockup-id
        { "status"     := status }
        (enforce (= status STATUS_REQUESTED) "request is not open")
        (update lockups lockup-id
          { "status" : STATUS_REJECTED })
        (format "request {} rejected" [lockup-id])
      )
    )
  )


  (defun approve-helper:string (lockup-id:string)
    (require-capability (OPS))
    (with-read lockups lockup-id
      { "status"     := status }
      (if (= status STATUS_REQUESTED)
        (update lockups lockup-id
          { "status" : STATUS_APPROVED })
        "skipping case"
      )
    )
  )

  (defun approve-all:[string] ()
    (with-capability (OPS)
      (map (approve-helper) (list-lockup-accounts))
    )
  )

  (defun read-lockup:object{lockup} (reservation-account:string)
    (read lockups reservation-account)
  )

  (defun read-all-lockups:[object{lockup}] ()
    (map (read lockups) (list-lockup-accounts))
  )

  (defun list-lockup-accounts:[string] ()
    (keys lockups)
  )

  (defun read-lockup-option:object{lockup-option} (name:string)
    (read lockup-options name)
  )

  (defun list-lockup-options:[string] ()
    (keys lockup-options)
  )

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [ (create-table lockups)
    (create-table lockup-options)

    (insert lockup-options ASAP_LOCKUP_LENGTH { 'length: 0.0, 'kda-return: 0.0, 'kdx-return: 0.0 })
    (insert lockup-options "3 months" { 'length: (days 90), 'kda-return: 0.01, 'kdx-return: 0.04 })
    (insert lockup-options "6 months" { 'length: (days 180), 'kda-return: 0.03, 'kdx-return: 0.17 })
    (insert lockup-options "9 months" { 'length: (days 270), 'kda-return: 0.05, 'kdx-return: 0.2 })
    (insert lockup-options "12 months" { 'length: (days 360), 'kda-return: 0.07, 'kdx-return: 0.23 })
    (insert lockup-options "15 months" { 'length: (days 450), 'kda-return: 0.09, 'kdx-return: 0.28 })
    (insert lockup-options "18 months" { 'length: (days 540), 'kda-return: 0.11, 'kdx-return: 0.45 })
    (insert lockup-options "21 months" { 'length: (days 630), 'kda-return: 0.13, 'kdx-return: 0.49 })
    (insert lockup-options "24 months" { 'length: (days 720), 'kda-return: 0.15, 'kdx-return: 0.54 })

    (init)
  ]
)
