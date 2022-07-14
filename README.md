# Kaddex Smart Contracts

This repository groups together all of the Kaddex smart contracts in a single place. The repository structure is as follows:

* The `kadenaswap` directory contains our fork of the Kadenaswap codebase. Major changes include support for Uniswap v2-style TWAP oracles which can be tracked across multiple pairs and support for global exchange-wide fees (equivalent to Uniswap's `_mintFee`, used for the staking rewards). Also contains the gas station module code.
* The `wrapper` directory contains the wrapper around liquidity positions that powers the liquidity mining program. Also includes the KDX token code and associated helper modules in the `wrapper/tokens` directory.
* The `staking` directory includes the contract code for the staking program.
* The `dao` directory includes the initial versions of the DAO related contracts responsible for keeping track of voting power and proposals.
* The `lockup` directory contains the code for the vaulting program for the first sale participants.

Test data in the form of `repl` files are included in each directory, which includes information on how to deploy and configure the initil state of the contracts.
