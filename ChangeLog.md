# simpoole

## 0.3.0

* Introduce Simpoole.Monad module
  * MonadPool to abstract over pooled resources in a monad (or transformer stack)
  * PoolT to interpret MonadPool, but also to help with re-entrance problems

## 0.2.0

* Make idle timeout optional
* Overhaul newPool function to be able to produce limited and unlimited pools alike

## 0.1.0

* Expose helper functions: acquireResource, returnResource, destroyResource
* Track number of idle resources as part of metrics
* Unify pool settings into Settings type
* Allow configuration of "return policy" which helps to optimize the pool

## 0.0.1

* Fix max live metric

## 0.0.0

This is the root version.
