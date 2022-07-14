(module public-sale-test GOV

  (use coin)
  (use util.guards)

  ;; -------------------------------
  ;; Schemas and Tables

  (defschema whitelist
    account:string
    guard:guard
    deleted:bool)

  (defschema sale
    name:string
    type:string
    startDate:time
    endDate:time
    kdxLimitPerAccount:decimal
    kdxSaleSupply:decimal
    kdxUsdRatio:decimal
    status:string)

  (defschema reservation
    sale:string
    account:string
    guard:guard
    timestamp:time
    usedToken:string
    amountUsedToken:decimal
    amountUsd:decimal
    amountKdx:decimal
    status:string)

  (defschema kdaUsdPrice
    price:decimal)

  (defschema kdaUsdPriceHistory
    oldPrice:decimal
    newPrice:decimal
    timestamp:time)

  (defschema cumulativeKdxSaleAmount
    cumulativeAmount:decimal)

  (deftable whitelists:{whitelist})
  (deftable sales:{sale})
  (deftable reservations:{reservation})
  (deftable kdaUsdPrices:{kdaUsdPrice})
  (deftable kdaUsdPriceHistories:{kdaUsdPriceHistory})
  (deftable cumulativeKdxSaleAmounts:{cumulativeKdxSaleAmount})

  ;; -------------------------------
  ;; Capabilities

  (defcap GOV ()
    (enforce-guard
      (keyset-ref-guard 'public-sale-test-keyset)))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'public-sale-test-ops)))

  (defcap RESERVE
    ( sale:string
      timestamp:time
      saleType:string)
    "Reserve event for kdx reservation"
    @event
    (with-read sales sale{
     "type":=type,
     "status":=status,
     "startDate":=startDate,
     "endDate":=endDate
     }
     (enforce (= type saleType) "Wrong sale type")
     (enforce (= status CREATED) "Sale status not created")
     (enforce (>= timestamp startDate) "StartDate error")
     (enforce (<= timestamp endDate) "EndDate error")
   )
  )

  (defcap RESERVE_REQUIREMENTS
    ( sale:string
      account:string
      amountKdx:decimal)
      (let
        (
          (availableSupply (available-supply sale))
          (availableAccountAllocation (kdx-allocation-account-available sale account))
        )
        (enforce (<= amountKdx availableSupply) "Total supply exceeded")
        (enforce (<= amountKdx availableAccountAllocation) "Total account allocation exceeded")
      )
  )

  ;; -------------------------------
  ;; Constants

  ;  Sale types
  (defconst ON-CHAIN:string 'on-chain)
  (defconst OFF-CHAIN:string 'off-chain)

  ;  Sale statuses
  (defconst CREATED:string 'created)
  (defconst CANCELED:string 'canceled)
  (defconst SUCCEEDED:string 'succeeded)
  (defconst FAILED:string 'failed)

  ;  Reservation statuses
  (defconst STATUS_REQUESTED:string 'requested)
  (defconst STATUS_APPROVED:string 'approved)
  (defconst STATUS_REJECTED:string 'rejected)

  ; KdaUsd price table row key
  (defconst KDA_USD_ROW_KEY 'kdausd)

  ; Kaddex bank account
  (defconst KDX_BANK:string 'public-sale-test-kdx-bank)

  (defun kdx-bank-guard () (create-module-guard "kaddex-test-admin"))

  (defun init ()
    (with-capability (GOV)
      (coin.create-account KDX_BANK (kdx-bank-guard))
      (insert kdaUsdPrices KDA_USD_ROW_KEY
        { "price" : 0.0 })
    )
  )

  (defun add-whitelist:string (account:string)
    @doc   "Add account to whitelist"
    (with-capability (OPS)
      (let
        (
          (g (at 'guard (coin.details account)))
        )
        (insert whitelists account
          { "account"    : account
          , "guard"      : g
          , "deleted"    : false
          })
        (format "{} added to whitelist" [account])
      )
    )
  )

  (defun delete-from-whitelist:string (account:string)
    @doc   "Remove account from whitelist"
    (with-capability (OPS)
      (update whitelists account {"deleted":true})
      (format "{} deleted from whitelist" [account])
    )
  )

  (defun create-sale:string (name:string type:string startDate:time endDate:time kdxUsdRatio:decimal kdxLimitPerAccount:decimal kdxSaleSupply:decimal)
    @doc   "Create sale with parameters"
    (enforce (< 0.0 kdxUsdRatio) "KDX/USD ratio is not a positive number")
    (enforce (< 0.0 kdxLimitPerAccount) "KDX limit per address is not a positive number")
    (enforce (< 0.0 kdxSaleSupply) "KDX sale supply is not a positive number")
    (enforce (or (= type ON-CHAIN) (= type OFF-CHAIN)) "Sale type not found")
    (with-capability (OPS)
        (insert cumulativeKdxSaleAmounts name
          { "cumulativeAmount": 0.0 })
        (insert sales name
          { "name"               :name
          , "type"               :type
          , "startDate"          :startDate
          , "endDate"            :endDate
          , "kdxUsdRatio"        :kdxUsdRatio
          , "kdxLimitPerAccount" :kdxLimitPerAccount
          , "kdxSaleSupply"      :kdxSaleSupply
          , "status"             :CREATED
          })
        (format "sale {} created" [name])
    )
  )

  (defun update-kda-usd-price:string (price:decimal)
    @doc   "Update kda-usd price - Simplified oracle to handle on-chain reservation"
    (enforce (< 0.0 price) "price is not a positive number")
      (with-capability (OPS)
        (with-read kdaUsdPrices KDA_USD_ROW_KEY {
          "price":=oldPrice}
            (let
              ((tx-id (hash {"price": price, "oldPrice": oldPrice, "salt": (curr-time)})))
              (update kdaUsdPrices KDA_USD_ROW_KEY {"price":price})
              (insert kdaUsdPriceHistories tx-id
                {
                  "oldPrice": oldPrice
                , "newPrice": price
                , "timestamp":(curr-time)
                })
              (format "Kda/Usd price updated: old price {} | new price {}" [oldPrice, price])
            )
        )
      )
  )

  (defun reserve-off-chain:string (sale:string txHash:string account:string usedToken:string amountUsedToken:decimal amountUsd:decimal amountKdx:decimal timestamp:time)
    @doc   "Add reservation handled off-chain"
    (with-capability (OPS)
      (with-capability (RESERVE sale timestamp OFF-CHAIN)
       (with-capability (RESERVE_REQUIREMENTS sale account amountKdx)
         (let
           (
             (g (at 'guard (coin.details account)))
           )
           (insert reservations (format "{}-{}" [account, txHash])
             { "sale"           : sale
             , "account"        : account
             , "usedToken"      : usedToken
             , "amountUsedToken": amountUsedToken
             , "amountUsd"      : amountUsd
             , "amountKdx"      : amountKdx
             , "timestamp"      : timestamp
             , "guard"          : g
             , "status"         : STATUS_REQUESTED
             })
             (with-read cumulativeKdxSaleAmounts sale{
               "cumulativeAmount":=cumulativeAmount
               }
               (update cumulativeKdxSaleAmounts sale {"cumulativeAmount": (+ cumulativeAmount amountKdx)})
              )
           (format "{} reserved {} KDX on {}" [account, amountKdx, sale])
          )
        )
      )
    )
  )

  (defun reserve-on-chain:string (sale:string account:string amountKda:decimal)
    @doc   "Add reservation directly on-chain"
    (with-capability (RESERVE sale (curr-time) ON-CHAIN)
      (with-read sales sale {
        "kdxUsdRatio":= kdxUsdRatio }
        (with-read whitelists account {
          "guard":= guard}
          (let*
            ( (tx-id (hash {"sale": sale, "account": account, "amountKda": amountKda, "salt": (curr-time)}))
              (amountUsd (* amountKda (kda-current-usd-price)))
              (amountKdx (/ amountUsd kdxUsdRatio))
              (isAccountWhitelisted (is-account-whitelisted account))
            )
            (enforce isAccountWhitelisted "Account not whitelisted")
            (with-capability (RESERVE_REQUIREMENTS sale account amountKdx)
              (coin.transfer account KDX_BANK amountKda)
              (insert reservations (format "{}-{}" [account, tx-id])
                { "sale"           : sale
                , "account"        : account
                , "usedToken"      : "KDA"
                , "amountUsedToken": amountKda
                , "amountUsd"      : amountUsd
                , "amountKdx"      : amountKdx
                , "timestamp"      : (curr-time)
                , "guard"          : guard
                , "status"         : STATUS_REQUESTED
                })
                (with-read cumulativeKdxSaleAmounts sale{
                  "cumulativeAmount":=cumulativeAmount
                  }
                  (update cumulativeKdxSaleAmounts sale {"cumulativeAmount": (+ cumulativeAmount amountKdx)})
                 )
              (format "{} reserved {} KDX on {}" [account, amountKdx, sale])
            )
          )
        )
      )
    )
  )

  (defun reject-on-chain:string (reservation-id:string)
    @doc   "Reject on-chain reservation with refund"
    (with-capability (OPS)
      (with-read reservations reservation-id
        { "sale"       := sale
        , "status"     := status
        , "amountUsedToken" := amount-kda
        , "account"    := account }
        (with-read sales sale
          {
            "type" := saleType
          }
          (enforce (= saleType ON-CHAIN) "sale type invalid")
          (enforce (= status STATUS_REQUESTED) "request is not open")
          (update reservations reservation-id
            { "status" : STATUS_REJECTED })
          (install-capability (coin.TRANSFER KDX_BANK account amount-kda))
          (coin.transfer KDX_BANK account amount-kda)
          (format "request {} rejected" [reservation-id])
        )
      )
    )
  )

  (defun reject-off-chain:string (reservation-id:string)
   @doc   "Reject off-chain reservation - Refund will be handled off-chain"
    (with-capability (OPS)
      (with-read reservations reservation-id
        { "sale"       := sale
        , "status"     := status }
        (with-read sales sale
          {
            "type" := saleType
          }
          (enforce (= saleType OFF-CHAIN) "sale type invalid")
          (enforce (= status STATUS_REQUESTED) "request is not open")
          (update reservations reservation-id
            { "status" : STATUS_REJECTED })
          (format "request {} rejected" [reservation-id])
        )
      )
    )
  )

  (defun approve:string (reservation-id:string)
    @doc   "Approve reservation"
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
    @doc   "Approve all reservation using helper"
    (with-capability (OPS)
      (map (approve-helper) (get-tx-ids))
    )
  )

  (defun end-sale (sale:string status:string)
    @doc   "End sale by setting properly status"
    (enforce (or (= status SUCCEEDED) (= status FAILED) (= status CANCELED)) "Sale status not found")
    (with-capability (OPS)
      (update sales sale {
        "status":status
        })
    )
  )

  (defun fetch-reservations:[object{reservation}](sale:string)
    @doc   "Get all reservations for specified sale"
    (select reservations (where 'sale (= sale)))
  )

  (defun fetch-account-reservations:[object{reservation}](sale:string account:string)
    @doc   "Get all account reservations for specified sale"
    (select reservations (and? (where 'sale (= sale)) (where 'account (= account))))
  )

  (defun kdx-reserved-account:decimal(sale:string account:string)
    @doc   "Get total KDX reserved for account in specified sale"
      (fold (+) 0.0 (map (at 'amountKdx) (fetch-account-reservations sale account)))
  )

  (defun kdx-allocation-account-available:decimal(sale:string account:string)
    @doc   "Get remaining KDX allocation for account in specified sale"
    (with-read sales sale{
      "kdxLimitPerAccount":=kdxLimitPerAccount
      }
      (- kdxLimitPerAccount (kdx-reserved-account sale account))
    )
  )

  (defun kdx-reserved:decimal(sale:string)
    @doc   "Get total KDX reserved in specified sale"
    (at 'cumulativeAmount (read cumulativeKdxSaleAmounts sale))
  )

  (defun available-supply:decimal(sale:string)
    @doc   "Get remaining KDX supply in specified sale"
    (with-read sales sale{
      "kdxSaleSupply":=kdxSaleSupply
      }
      (with-read cumulativeKdxSaleAmounts sale{
        "cumulativeAmount":=cumulativeAmount
        }
        (- kdxSaleSupply cumulativeAmount)
      )
    )
  )

  (defun kda-current-usd-price:decimal()
    @doc   "Get current KDA/USD price"
    (at 'price (read kdaUsdPrices KDA_USD_ROW_KEY))
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

  (defun read-all-kda-usd-price-history ()
    @doc "Returns all KDA/USD price update"
    (map (read kdaUsdPriceHistories) (keys kdaUsdPriceHistories))
  )

  (defun is-account-whitelisted (account:string)
    @doc "Returns true if an account is whitelisted otherwise false"
    (with-read whitelists account {
      "deleted":= deleted
      }
      (!= deleted true)
    )
  )

  (defun read-sale:object{sale} (sale:string)
    @doc "Returns sale object"
    (read sales sale)
  )

  ; --------------------------------------------------------------------------
  ; Utils

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))

)

(if (read-msg 'upgrade)
  ["upgrade"]
  [
    (create-table whitelists)
    (create-table sales)
    (create-table reservations)
    (create-table kdaUsdPrices)
    (create-table kdaUsdPriceHistories)
    (create-table cumulativeKdxSaleAmounts)
    (init)
  ]
)
