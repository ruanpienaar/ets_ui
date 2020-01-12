# [ETS](https://erlang.org/doc/man/ets.html)
[![Build# Status](https://travis-ci.org/ruanpienaar/ets_ui.svg?branch=master)](https://travis-ci.org/ruanpienaar/ets_ui)
[![Coverage Status](https://coveralls.io/repos/github/ruanpienaar/ets_ui/badge.svg?branch=master)](https://coveralls.io/github/ruanpienaar/ets_ui?branch=master)

## Synopsis

This project allows for querying Erlang ETS tables over http, so that other
languages/mechanisms can easily get database results.

## Example

This project can be included as a dependancy. 
It starts the cowboy application on a specified port per application env `http_port`, 
or lets the underlying OS decide what port to use IE: use any unused port.

To create a few tables with some example context run the following:

```ets_ui_util:dummy_table().
```

or just create ETS tables yourself and play around on the ui

## Motivation

Agregating results from multiple erlang nodes in one place.

ETS querying capability over HTTP. 
* TODO: client-mode/cluster-mode *
cluster mode runs queries on it's child client-mode machines

## Installation

```make shell
```

## API Reference

[Docs](https://documenter.getpostman.com/view/6263596/SWDzgMRG?version=latest)

## Tests

```make test
```

## Contributors

4 spaces no tabs
please use nice/readible names for vars/funcs
please run checks locally when work is considered finished

## License

[Apache License 2.0](https://github.com/ruanpienaar/ets_ui/blob/master/LICENSE)