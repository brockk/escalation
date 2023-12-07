
# escalation 0.1.6

* Implement efficient simulation comparison method of Sweeting et al. in simulate_compare()
* Implement BOIN12 design
* Add MTD / OBD selectors for TPI, mTPI, mTPI2 and BOIN12 for final dose choice at end of a trial

# escalation 0.1.5

* Add mTPI2 dose-finding method
* Fixed some bugs

# escalation 0.1.4

* Added Neuenschwander, Branson & Sponer's method for phase I.
* Added Ji et al.'s TPI & mTI methods for phase I.
* Added Thall & Cook's EffTox method for phase I/II.
* Added Wages & Tait's method for phase I/II.

# escalation 0.1.3

* Mainly small changes to comply with dplyr v1.0.
* simulations and crystallised_dose_paths given summary method
* NOTE squash


# escalation 0.1.2

* Methods for simulation and calculating dose-paths added.
* A bit more on the general selector interface.
* Everything new has tests, and more tests on old stuff.


# escalation 0.1.1

* Initial release.
* dfcrm, BOIN, 3+3 models supported.
* several objects included to facilitate stopping at different events.
* class for a fixed escalation plan included, as sometimes used with CRM.
