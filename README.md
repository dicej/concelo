Concelo - Real-time synchronization of encrypted content
========================================================

[![Build Status](https://travis-ci.org/Concelo/concelo.svg?branch=master)](https://travis-ci.org/Concelo/concelo)

Concelo is a system for synchronizing encrypted application state
among multiple peers.  It is designed to act as a transparent
encryption layer between an application and a low-latency
key-value store hosted by a possibly untrusted party.

Its goals include:

  * `Scalability`: any eventually consistent distributed key-value
    store may be used

  * `Privacy`: storage provider(s) have no means to decrypt or modify the
    content they host

  * `Reliability`: a Merkel-tree-based state synchronization protocol
    ensures that subscribers will eventually converge on the published
    state regardless of intermittent packet or connection loss, IP
    address changes, etc.

  * `Low latency`: the synchronization protocol naturally allows for
    obsolete elements of state to be skipped as newer data becomes
    available

Please see the [wiki](https://github.com/Concelo/concelo/wiki) for details.
