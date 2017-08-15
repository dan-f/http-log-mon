# http-log-mon

## Installation

First, you'll need to install GHC (by way of [Stack](https://docs.haskellstack.org/en/stable/README/)) to get this program running:

- [Stack installation instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Next, you'll want to clone the repo:

```sh
$ git clone https://github.com/totally-not-dan/http-log-mon.git && cd http-log-mon
```

Next, build it!

```sh
$ stack build
```

Finally, run it!

```sh
$ stack exec http-log-mon-exe path/to/logfile.txt
```

## Testing

To run the tests, simply run:

```sh
$ stack test
```

Cool!
