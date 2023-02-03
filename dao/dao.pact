(namespace (read-msg 'ns ))

(module dao GOVERNANCE

  ; ------------------ Contracts -----------------------------

  (use coin)


  ; ------------------ Schemas --------------------------------

  (defschema proposal
    id:string
    title:string
    description:string
    account:string
    tot-approved:decimal
    tot-refused:decimal
    start-date:time
    end-date:time
    creation-date:time
    )
  (defschema vote
    proposal-id:string
    account:string
    vp:decimal
    action:string
    )

  ; ------------------ Tables ------------------------------------

  (deftable proposals-table:{proposal})
  (deftable votes-table:{vote})

  ; ------------------ Constants ---------------------------------

  (defconst MAX_PROPOSAL_DEBATE_PERIOD (days 14))

  (defconst VOTE_APPROVED "approved")
  (defconst VOTE_REFUSED "refused")

  ; ------------------ Capabilities -----------------------------

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-dao-admin )))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-dao-ops )))

  (defcap INTERNAL ()
    "mark some functions as internal only"
    true)

  (defcap ACCOUNT_GUARD ( account:string )
    @doc " Look up the guard for an account, required to withdraw from the contract. "
    (enforce-guard (at 'guard (kaddex.kdx.details account))) "Keyset Failure")

  (defcap PROPOSAL_PERIOD
      ( proposal-id:string
        timestamp:time)
      "Reserve event for kdx reservation"
      @event
      (with-read proposals-table proposal-id {
        "start-date":=start-date,
       "end-date":=end-date
       }
       (enforce (>= timestamp start-date) "Proposal is not yet open")
       (enforce (<= timestamp end-date) "The proposal is closed")
     )
    )

  ; ------------------ Utility Functions -----------------------------


  (defun votes-table-key (account:string proposal-id:string)
    @doc "create id for insert and update vote-table"
    (format "{}-{}" [account proposal-id])
  )

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))

  (defun check-account-voted (account:string proposal-id:string)
    @doc "Checks if an account has already voted a specific proposal"
    (with-default-read votes-table (votes-table-key account proposal-id)
      {"account":""}
      {"account":=account}
      (if (= account "") false true)
      )
  )

  ; ------------------ Functions ------------------------------------


  (defun create-proposal:string
    (title:string description:string owner-account:string start-date:time end-date:time)
    @doc "Creates proposal with the parameters: `title` `description` `owner-account` `start-date` `end-date` and insert it into proposals-table"
    (with-capability (OPS)
    (enforce (< (curr-time) start-date) "Start Date shouldn't be in the past")
    (enforce (< start-date end-date) "End Date should be later than start date")
    (enforce (< (diff-time end-date start-date) MAX_PROPOSAL_DEBATE_PERIOD)
      "proposal period is too long")
    (enforce (!= "" title) "Title should be decleared")
    (enforce (!= "" description) "Description should be decleared")
    (enforce (= "k:" (take 2 owner-account)) "only k: accounts allowed")

    (let
      (
        (id (hash title))
        )
      (insert proposals-table id{
        "id":id,
        "title": title,
        "description":description,
        "account":owner-account,
        "start-date":start-date,
        "creation-date":(curr-time),
        "end-date":end-date,
        "tot-approved": 0.0,
        "tot-refused":0.0
        })
        (format "{} - {} proposal from {} to {} has been created" [id title start-date end-date])
      )
    )
    )


  (defun vote-proposal-helper (proposal-id:string account:string action:string )
    @doc "Helper function for inserting vote of an account on a proposal"
    (require-capability (INTERNAL))
    (with-capability (PROPOSAL_PERIOD proposal-id (curr-time))
      (let ((vp (kaddex.aggregator.get-voting-power account)))
       (enforce (> vp 0.0) "You can not vote")
       (insert votes-table (votes-table-key account proposal-id) {
         "proposal-id":proposal-id
         ,"account":account
         ,"vp":vp
         ,"action":action
       })
      )
    )
  )

  (defun approved-vote:string (proposal-id:string account:string)
    @doc "Vote for a proposal as approved"
    (with-capability (ACCOUNT_GUARD account)
    (with-capability (INTERNAL)
    (with-capability (PROPOSAL_PERIOD proposal-id (curr-time))
    (let ((check-already-voted (check-account-voted account proposal-id)))
      (enforce (not check-already-voted)
        "This account has already voted to this proposal")
      )
    (let* (
      (vp (kaddex.aggregator.get-voting-power account))
    )
    (enforce (> vp 0.0)
      "This account does not have voting power")
    (with-read proposals-table proposal-id {
       "tot-approved":= tot-approved
       }
        (update proposals-table proposal-id
          {"tot-approved": (+ tot-approved (floor (sqrt vp) 2))}) ;; Quadratic Voting
      )
        (vote-proposal-helper proposal-id account VOTE_APPROVED)
      (format "Account {} APPROVED the '{}' proposal" [account proposal-id])
      )
    )
    )
    )
  )

  (defun refused-vote:string (proposal-id:string account:string)
    @doc "Vote for a proposal as refused"
    (with-capability (ACCOUNT_GUARD account)
    (with-capability (INTERNAL)
    (with-capability (PROPOSAL_PERIOD proposal-id (curr-time))
    (let ((check-already-voted (check-account-voted account proposal-id)))
      (enforce (not check-already-voted)
        "This account has already voted to this proposal")
      )
    (let* (
      (vp (kaddex.aggregator.get-voting-power account))
    )
    (enforce (> vp 0.0)
       "This account does not have voting power")
    (with-read proposals-table proposal-id {
       "tot-refused":= tot-refused
       }
       (update proposals-table proposal-id
         {"tot-refused": (+ tot-refused (floor (sqrt vp) 2))})  ;; Quadratic Voting
      )
        (vote-proposal-helper proposal-id account VOTE_REFUSED)
      (format "Account {} REFUSED the '{}' proposal" [account proposal-id])
      )
    )
    )
    )
  )

  (defun get-account-data:object (account:string)
    @doc "Function that returns information (voting-power, multiplier and staked-amount) about a specific account"
    (let*
      (
        (account-data (kaddex.aggregator.get-account-data account))
        (vp (at 'vp account-data))
        )
        {'vp:(floor (sqrt vp) 5), 'multiplier:(at 'multiplier account-data), 'staked-amount:(at 'staked-amount account-data)}
      )
  )

  (defun read-proposal (proposal-id:string)
    @doc "Get proposal info giving a specific proposal-id"
    (read proposals-table proposal-id)
  )

  (defun read-all-proposals ()
    @doc "Get all proposals info"
    (map (read proposals-table) (get-proposals-ids))
  )

  (defun get-proposals-ids ()
    @doc "read-all-proposals helper function"
    (keys proposals-table)
  )

  (defun read-account-votes (account:string)
    @doc "get vote info for each proposal that account has voted in the votes-table"
    (select votes-table (where 'account (= account)) )
  )

  (defun read-account-vote-proposal (account:string proposal:string)
    @doc "Get vote info of account on a specific proposal"
    (select votes-table (and? (where 'account (= account)) (where 'proposal-id (= proposal))))
  )
)

; ------------------ Defining tables ---------------------------------

(if (read-msg 'upgrade)
  ["upgrade"]
  [
    (create-table proposals-table)
    (create-table votes-table)
  ]
)
