# stm-loader

Haskell code to fork off a separate thread that loads and unloads "resources" which could be anything but are typically chunks
of data loaded from disk or the network.

- Communicate via STM: put a request in one STM variable, and once the resources are loaded they appear in a separate STM variable
- Support multiple threads and task groups via [import Control.Concurrent.Async.Pool](https://hackage.haskell.org/package/async-pool)
- Dependencies are specified declaratively. Loads and unloads are sequenced so that a resource is not loaded before it's dependencies or unloaded after it's dependencies.
