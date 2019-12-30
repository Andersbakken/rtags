# Adding a new test
To add a new test in this folder, just add a new folder with a
descriptive name with some sources and an `expectation.json` file with
some commands to run through `rc` and the expected resulting
locations.

The directory must start with a specific word to classify the test, those are:

* Location
* Parsing
* Completion
* Output

If it doesn't start with one of those it is considered to be a _Location_ test.

Those types of tests are handled with the test file _test\_misc.py_. For
bigger tests consider using a separate test file.
