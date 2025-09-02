# sdmTMBextra 0.0.5

* Fix the barrier model implementation. At some point, INLA/INLAspacetime changed
  the SPDE matrix output used in the barrier calculations. sdmTMB now deals
  with these new matrices appropriately and has been tested to match the new
  INLAspacetime implementation. Unit tests should catch this in the future
  should it happen again. The update is mainly over in sdmTMB.

# sdmTMBextra 0.0.4

* Remove `dharma_residuals()` since this is now directly in sdmTMB.
