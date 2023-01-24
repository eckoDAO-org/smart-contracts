;;
;;
;;

(namespace (read-msg 'ns))

(module liquidity-helper GOVERNANCE

  (defcap GOVERNANCE () true)

  (defschema account
      account:string
    guard:guard)
  (deftable temporary-accounts:{account}) ;; keyed by exchange pair key

  (defcap ACCOUNT_ACCESS (pair-key:string)
    true)
  (defun enforce-account-access (pair-key:string)
    (require-capability (ACCOUNT_ACCESS pair-key)))
  (defun create-account-guard (pair-key:string)
    (create-user-guard (enforce-account-access pair-key)))

  (defun get-pair-key:string (tokenA:module{fungible-v2} tokenB:module{fungible-v2})
    (exchange.get-pair-key tokenA tokenB))

  (defun get-or-create-temp-account:{account} (tokenA:module{fungible-v2} tokenB:module{fungible-v2})
         (let ((pair-key (get-pair-key tokenA tokenB)))
           (with-default-read temporary-accounts pair-key
             { 'account: "", 'guard: (create-account-guard pair-key) }
             { 'account := account, 'guard := guard }
             (if (!= account "")
                 { 'account: account, 'guard: guard }
                 (let* ((account-name (create-principal guard))
                        (account-rec { 'account: account-name, 'guard: guard }))
                   (tokenA::create-account account-name guard)
                   (tokenB::create-account account-name guard)
                   (with-capability (ACCOUNT_ACCESS pair-key)
                     (enforce-guard (at 'guard (tokenA::details account-name)))
                     (enforce-guard (at 'guard (tokenB::details account-name))))
                   (insert temporary-accounts pair-key
                           account-rec)
                   account-rec
                   )
                 )
             )
           ))

  (defun get-one-sided-liquidity-swap-amount:decimal (
                                                      A:decimal ;; reserves of tokenA
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

  (defun get-add-liquidity-token-amount-after-swap:decimal (
                                                            amountA-total:decimal
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
    (enforce (>= slippage 1.0) "get-add-liquidity-token-amount-after-swap: Slippage values below 1.0 will cause the transaction to fail")
    (let*
        ( (p (exchange.get-pair tokenA tokenB))
         (reserveA (exchange.reserve-for p tokenA))
          (amountB-in (exchange.truncate tokenB (get-one-sided-liquidity-swap-amount reserveA amountA-total)))
          (alloc {  'token-out: tokenA
                                , 'token-in: tokenA
                                , 'out: amountB-in
                                , 'in: 0.0
                                , 'idx: 0
                                , 'pair: p
                                , 'path: [tokenB]
                                })
          (out-result (exchange.compute-out [alloc] tokenB))
          )
      ;; multiply by the slippage in case the price changes between the user
      ;; querying this function and calling the function below
      (exchange.truncate tokenB (* slippage (at 'out (at 0 out-result))))
      )
    )

  (defun add-liquidity-one-sided:object (
                                         token-in:module{fungible-v2}
                                         token-other:module{fungible-v2}
                                         amount-in-total:decimal
                                         amount-in-min:decimal
                                         amount-other-min:decimal
                                         sender:string
                                         sender-guard:guard
                                         to:string
                                         to-guard:guard
                                         use-wrapper:bool
                                         )
    "Adds liquidity to the tokenA/tokenB pair using only tokenA. Will automatically swap about half of the amountA-total into tokenB to provide the liquidity."
    (enforce (and (and (> amount-in-total 0.0) (>= amount-in-min 0.0)) (>= amount-other-min 0.0)) "add-liquidity-one-sided: Values must be positive")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= to "") "Invalid target")
    (token-in::enforce-unit amount-in-total)
    (token-in::enforce-unit amount-in-min)
    (token-other::enforce-unit amount-other-min)
    (let* ( (order-ok (exchange.is-canonical token-in token-other))
           (p (exchange.get-pair token-in token-other))
            (reserve-in (exchange.reserve-for p token-in))
            (swap-amount-in (exchange.truncate token-in (get-one-sided-liquidity-swap-amount reserve-in amount-in-total)))
            (amount-in-liq (exchange.truncate token-in (- amount-in-total swap-amount-in)))
            )
      (enforce (>= amount-in-liq amount-in-min) "Insufficient A amount")
      (let* ( ;; perform the swap
             (swap-result (exchange.swap-exact-in swap-amount-in 0.0 [token-in token-other] sender sender sender-guard))
             (amount-other (at 'amount (at 1 swap-result)))
             )
        (enforce (>= amount-other amount-other-min) "Insufficient B amount")
        ;; add the liquidity as normal
        (if use-wrapper
            (if order-ok
                (exchange.add-liquidity token-in token-other amount-in-liq amount-other amount-in-min amount-other-min sender to to-guard)
                (exchange.add-liquidity token-other token-in amount-other amount-in-liq amount-other-min amount-in-min sender to to-guard))
            (if order-ok
                (wrapper.add-liquidity token-in token-other amount-in-liq amount-other amount-in-min amount-other-min sender to to-guard)
                (wrapper.add-liquidity token-other token-in amount-other amount-in-liq amount-other-min amount-in-min sender to to-guard))
            )
        )
      )
    )

  (defun remove-liquidity-one-sided:object (
                                            token-out:module{fungible-v2}   ;; the token the user wants to receive
                                            token-other:module{fungible-v2} ;; the other token of the pool
                                            requested-liquidity:decimal     ;; liquidity being removed
                                            amount-out-min-liquidity:decimal          ;; minimum amount of `token-out` that needs to be received after remove-liquidity
                                            amount-other-min-liquidity:decimal        ;; minimum amount of `token-other` that needs to be received after remove-liquidity
                                            swap-slippage:decimal           ;; the max slippage used for the swap of token-other for token-out. a value of 0.95 gives 5% slippage (1 - slippage)
                                            sender:string
                                            to:string
                                            to-guard:guard
                                            use-wrapper:bool                ;; whether to use kaddex.wrapper or kaddex.exchange
                                            wants-kdx-rewards:bool          ;; if using the wrapper, whether the user wants the kdx rewards or not
                                            )
    "Removes liquidity from the tokenA/tokenB pair and swaps all tokenB for tokenA afterwards."
    (enforce (> requested-liquidity 0.0) "remove-liquidity-one-sided: Liquidity must be positive")
    (enforce (!= sender "") "Invalid sender")
    (enforce (!= to "") "Invalid target")
    (enforce (or (not wants-kdx-rewards) (and use-wrapper wants-kdx-rewards)) "KDX rewards are only available via the wrapper")
    (enforce (and (> swap-slippage 0.0) (<= swap-slippage 1.0)) "remove-liquidity-one-sided: Invalid slippage")
    (tokens.enforce-unit (exchange.get-pair-key token-out token-other) requested-liquidity)
    (token-out::enforce-unit amount-out-min-liquidity)
    (token-other::enforce-unit amount-other-min-liquidity)
    (let* (
           (pair-key (get-pair-key token-out token-other))
           (temp-account (get-or-create-temp-account token-out token-other))
           (initial-temp-out-balance (token-out::get-balance (at 'account temp-account)))
           (initial-temp-other-balance (token-other::get-balance (at 'account temp-account)))
           (order-ok (exchange.is-canonical token-out token-other))
           (remove-result (if use-wrapper
                              (if order-ok
                                  (wrapper.remove-liquidity token-out token-other requested-liquidity amount-out-min-liquidity amount-other-min-liquidity sender (at 'account temp-account) (at 'guard temp-account) wants-kdx-rewards)
                                  (wrapper.remove-liquidity token-other token-out requested-liquidity amount-other-min-liquidity amount-out-min-liquidity sender (at 'account temp-account) (at 'guard temp-account) wants-kdx-rewards))
                              (if order-ok
                                  (exchange.remove-liquidity token-out token-other requested-liquidity amount-out-min-liquidity amount-other-min-liquidity sender (at 'account temp-account) (at 'guard temp-account))
                                  (exchange.remove-liquidity token-other token-out requested-liquidity amount-other-min-liquidity amount-out-min-liquidity sender (at 'account temp-account) (at 'guard temp-account)))
                              ))
           )
      (let* ( ;; perform the swap
             (amount-out-before (if use-wrapper (if order-ok (at 'amountA remove-result) (at 'amountB remove-result)) (if order-ok (at 'amount0 remove-result) (at 'amount1 remove-result))))
             (amount-other (if use-wrapper (if order-ok (at 'amountB remove-result) (at 'amountA remove-result)) (if order-ok (at 'amount1 remove-result) (at 'amount0 remove-result))))
             (pair (exchange.get-pair token-out token-other))
             (reserve-out (exchange.reserve-for pair token-out))
             (reserve-other (exchange.reserve-for pair token-other))
             (quote-out (exchange.quote amount-other reserve-other reserve-out))
             (amount-out-min-swap (exchange.truncate token-out (* swap-slippage quote-out)))
             (pair-account (at 'account (exchange.get-pair token-out token-other)))
             )

        (install-capability (token-other::TRANSFER (at 'account temp-account) pair-account amount-other))
        (with-capability (ACCOUNT_ACCESS pair-key)
          (let* ((swap-result (exchange.swap-exact-in amount-other amount-out-min-swap [token-other token-out] (at 'account temp-account) to to-guard))
                 (swap-amount (at 'amount (at 1 swap-result))))
            (install-capability (token-out::TRANSFER (at 'account temp-account) to amount-out-before))
            (token-out::transfer-create (at 'account temp-account) to to-guard amount-out-before)
            (let (
                  (final-temp-out-balance (token-out::get-balance (at 'account temp-account)))
                  (final-temp-other-balance (token-other::get-balance (at 'account temp-account)))
                  )
              (enforce (and (= initial-temp-out-balance final-temp-out-balance)
                            (= initial-temp-other-balance final-temp-other-balance)) "Conservation of mass")
              { 'remove-result: remove-result, 'swap-result: swap-result, 'total-amount: (exchange.truncate token-out (+ swap-amount amount-out-before)) }
              )
            )
          )
        )
      )
    )
  )

;; FIXME: possible "DoS" by create-account the principal acc before this contract for only one or two tokens -- if the account exists, just validate principal yourself

(create-table temporary-accounts)
