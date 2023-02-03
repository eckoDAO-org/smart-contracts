(namespace (read-msg 'ns ))
(module aggregator GOVERNANCE

  (use util.guards)

  (defschema staking-schema
      account:string
      start-time:time
      staked-amount:decimal
      shift:decimal)

  (defschema account-data
    vp:decimal
    multiplier:decimal)

  (defschema privilege
    guards:[guard]
    action:string)

  (deftable staking-aggregator-table:{staking-schema})
  (deftable privilege-table:{privilege})

  (defconst ACTION_AGGREGATE_STAKE:string 'aggregate-stake)
  (defconst ACTION_AGGREGATE_UNSTAKE:string 'aggregate-unstake)

  (defconst Z:decimal 0.288507)
  (defconst S:decimal 0.0114976)
  (defconst C:decimal 0.001)

  (defconst LOCKUP-START-AGGREGATE-DATE:time (time "2022-03-14T20:00:00Z"))

  (defconst INTERVAL:integer 100000)

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-aggregator-admin )))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-aggregator-ops )))

  (defcap PRIVILEGE-GUARD (action:string)
      @event
      (enforce-privilege action)
    )

  (defcap AGGREGATE ()
    "mark some functions as internal only"
    true)



    (defun grant-privilege:string (g:guard action:string)
      @doc "This function grants a guard privilege to perform a specific action."
      (with-capability (OPS)
       (with-default-read privilege-table action
         {'guards:[],'action:action}
         {'guards:=guards-read, 'action:=action-read}
        (write privilege-table action{
          'guards:(+ guards-read [g]),
          'action:action
          }))
        )
    )

  (defun enforce-privilege:bool
      (action:string )
        @doc "checks if a guard has privilege on a given action"
      (let ((privilege-granted (check-privilege action)))
        (enforce privilege-granted (format "Could not obtain privilege on action {}" [action]))
      )
  )

  (defun check-privilege:bool (action:string)
    @doc "enforce-privilege utility function"
    (with-default-read privilege-table action
      { 'guards: [] }
      { 'guards := guards }
      (fold (or-guard) false guards)
    )
  )

  (defun or-guard:bool
    ( a:bool
      b:guard )
     (or a (try false (enforce-guard b)))
  )



  (defun insert-lockup-batch:string (lockup-reservations:[object])
   @doc "Vaulting data are inserted into `staking-aggregator-table` through the use of this function."
    (with-capability (OPS)
      (with-capability (AGGREGATE)
        (map (insert-lockup) lockup-reservations)
      )
    )
  )

  (defun insert-lockup:string (data:object)
    @doc "function helper to insert-lockup-batch"
    (require-capability (AGGREGATE))
      (let*
        ( (account (at 'account data))
          (amount (at 'amount data))
        )
        (write staking-aggregator-table account
          { 'account: account
          , 'staked-amount: amount
          , 'start-time: LOCKUP-START-AGGREGATE-DATE
          , 'shift: 0.0 })
        (format "Account {} inserted with amount {} kdx" [account, amount])
      )
  )

  (defun aggregate-stake:string (requesting-account:string amount:decimal)
    @doc "Verify whether an account is entering staking for the first time or not, if not, it checks if there is some amount already staked."
    (with-capability (PRIVILEGE-GUARD ACTION_AGGREGATE_STAKE)
      (with-capability (AGGREGATE)
        (with-default-read staking-aggregator-table requesting-account
          {"account":"", "start-time":(curr-time), "staked-amount":0.0, "shift":0.0}
          {"account":=account, "start-time":=start-time, "staked-amount":=staked-amount, "shift":=shift}

        (if (= account "")
          (create-new-stake-row requesting-account amount)
          (if (= staked-amount 0.0)
            (update-account-entering-staking-again account amount)
            (update-old-stake-row account amount start-time staked-amount shift)
            )
        )))
        )
      )

  (defun aggregate-unstake:string (requesting-account:string unstaked-amount:decimal)
    @doc "It's called every time a new action of unstaking is made by an account. Calls update-when-unstaking function to insert data into staking-aggregator-table"
    (with-capability (PRIVILEGE-GUARD ACTION_AGGREGATE_UNSTAKE)
      (with-capability (AGGREGATE)
        (with-default-read staking-aggregator-table requesting-account
          {"account":"", "start-time":(curr-time), "staked-amount":0.0, "shift":0.0}
          {"account":=account, "start-time":=start-time, "staked-amount":=staked-amount, "shift":=shift}
          (enforce (!= account "") "Account does not exist")
          (update-when-unstaking account unstaked-amount start-time staked-amount shift )
          ))
        )
      )



  (defun update-when-unstaking:string (account:string unstaked-amount:decimal start-time:time old-staked-amount:decimal shift:decimal)
    @doc  "Updates table when user unstake, totally or partially"
    (require-capability (AGGREGATE))
    (if (<= (- old-staked-amount unstaked-amount) 0.0)
      ;; "Total unstake"
      (update staking-aggregator-table account{
        "staked-amount":0.0,
        "shift":0.0
        })

        ;; "Partial unstake"
        (let*
          (
            (X (diff-time (curr-time) start-time))
            (curr-mult (getMultiplier X shift))
            (modified-mult (getMultiplierAfterPartialUnstake old-staked-amount curr-mult unstaked-amount))
            (updated-shift (getShift modified-mult X))
            (total-stake (- old-staked-amount unstaked-amount))
          )
          (update staking-aggregator-table account{
            "staked-amount":total-stake,
            "shift":updated-shift
            })
        )
      )
      (format "Account {} have just unstaked {}" [account unstaked-amount])
  )

  (defun create-new-stake-row:string (account:string staked-amount:decimal)
    @doc   "Create new account row into staking-aggregator-table"
    (require-capability (AGGREGATE))
    (insert staking-aggregator-table account{
      "account":account,
      "start-time":(curr-time),
      "staked-amount":staked-amount,
      "shift":0.0
      })
      (format "New row created for account {} staking {}" [account staked-amount])
    )

  (defun update-account-entering-staking-again:string (account:string staked-amount:decimal)
    @doc   "User entering staking again after previous total unstake"
    (require-capability (AGGREGATE))

    (update staking-aggregator-table account{
      "start-time":(curr-time),
      "staked-amount":staked-amount,
      "shift":0.0
      })

      (format "Old row updated for account {} that entered staking again with amount {} " [account staked-amount])

    )

  (defun update-old-stake-row:string (account:string staked-amount:decimal start-time:time old-staked-amount:decimal shift:decimal)
    @doc   "User adding more stake (already having some amount staked)"
    (require-capability (AGGREGATE))
      (let*
        (
          (X (diff-time (curr-time) start-time))
          (curr-mult (getMultiplier X shift))
          (modified-mult (getMultiplierAfterNewStake old-staked-amount curr-mult staked-amount))
          (updated-shift (getShift modified-mult X))
          (total-stake (+ old-staked-amount staked-amount))
        )
        (update staking-aggregator-table account{
          "staked-amount":total-stake,
          "shift":updated-shift
          })
        (format "Row updated for account {} total-staking {} and shift {} currMult {} X {}" [account total-stake updated-shift curr-mult X])
      )
  )

  (defun getMultiplier:decimal (X:decimal shift:decimal)
    @doc "multiplier formula"
    (require-capability (AGGREGATE))
    (let
      ((mult (round (+ (* S (^ (+ X shift) Z)) C) 10)))
       (if (> mult 2.5) 2.5 mult))
  )

  (defun getShift:decimal (multiplier:decimal X:decimal)
     @doc "shift formula"
     (require-capability (AGGREGATE))
     (let
       ((verified-multiplier (if (< multiplier C) C multiplier)))
       (round (- (^ (/ (- verified-multiplier C) S) (/ 1 Z)) X) 10)
     )
   )

  (defun getMultiplierAfterNewStake:decimal (old-staked-amount:decimal current-multiplier:decimal new-staked-amount:decimal)
    @doc "formula to balance shift and multiplier after a new action of staking is made"
    (require-capability (AGGREGATE))
    (let
      ((sum-stake (+ old-staked-amount new-staked-amount)))
      (+ (* (/ old-staked-amount sum-stake) current-multiplier) (* (/ new-staked-amount sum-stake) 0.001))
    )
  )

  (defun getMultiplierAfterPartialUnstake:decimal (old-staked-amount:decimal current-multiplier:decimal unstaked-amount:decimal)
    @doc "formula to balance shift and multiplier after a new action of unstaking is made"
    (require-capability (AGGREGATE))
    (* current-multiplier (- 1 (/ unstaked-amount old-staked-amount)))
  )




  (defun get-multiplier-by-account:decimal (account:string)
    @doc "returns multiplier value of a given account"
    (with-capability (AGGREGATE)
      (with-read staking-aggregator-table account
        {"account":=account, "start-time":=start-time, "staked-amount":=staked-amount, "shift":=shift}
        (getMultiplier (diff-time (curr-time) start-time) shift)
      )
    )
  )

  (defun get-voting-power:decimal (account:string)
    @doc "returns voting power value of a given account"
    (with-capability (AGGREGATE)
      (round (get-voting-power-by-staking account) 5)
    )
  )

  (defun get-voting-power-by-staking:decimal (account:string)
    @doc "get-voting-power helper function"
    (require-capability (AGGREGATE))
    (with-default-read staking-aggregator-table account
      {"account": "", "start-time": (curr-time), "staked-amount": 0.0, "shift": 0.0}
      {"account":=account, "start-time":=start-time, "staked-amount":=staked-amount, "shift":=shift}
      (let* (
        (X (diff-time (curr-time) start-time))
        (multiplier (getMultiplier X shift))
        )
        (* staked-amount multiplier)
      )
    )
  )

  (defun get-account-data:object{account-data} (account:string)
    @doc "Function that returns information (voting-power, multiplier and staked-amount) about a specific account"
    (with-read staking-aggregator-table account{
      "staked-amount":=staked-amount
      }
    (let*
      (
        (vp (get-voting-power account))
        (mult (if (= staked-amount 0.0) 0.0 (get-multiplier-by-account account)))
        )
      {'vp:vp, 'multiplier:mult, 'staked-amount:staked-amount}
    )
    )
  )


  (defun read-all-staking ()
    @doc "Get all staking info"
    (map (read staking-aggregator-table) (get-staking-ids))
  )

  (defun get-staking-ids ()
    @doc "read-all-staking helper function"
    (keys staking-aggregator-table)
  )


  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))


)

(if (read-msg 'upgrade)
  ["upgrade"]
  [
    (create-table staking-aggregator-table)
    (create-table privilege-table)
  ]
)
