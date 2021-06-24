# README
# OCaml Data Science Library

## About
This is a data science + machine learning library for OCaml, mainly intended for people wishing to learn how to implement machine learning algorithms in a functional programming language, and for use in personal projects. The following features are offered:

 1. Jupyter frontend for OCaml
 2. Load CSV files
 3. Matrix operations, from basic arithmetic to everything including row reduction, inverse, transpose, eigenvectors, determinants.
 4. Descriptive statistics for dataset, dataframe slicing and broadcasting.
 5. Simple syntax to split up a dataset into training, validation and test sets, apply a prewritten model, and evaluate the accuracy of the model on the dataset.
 6. The following machine learning algorithms: Logistic Regression, Polynomial Regression, K Nearest Neighbors, K Means Clustering, Naive Bayes, Decision Trees, Perceptron.
 8. A command line utility that allows users not proficient in OCaml programming to still use some common functionality, including basic data manipulation and application of models. 
 
## Installation

The installation of this library, along with the setting up the Jupyter kernel is described in `install.txt`.

## Make targets

Several `make` commands can be run in the home directory:

 1. `make build` - Compile the code
 2. `make test` - Run the test suite
 3. `make docs` - Generate `ocamldoc` documentation. We recommend looking for `index.html` inside the `_doc.private` folder that gets generated inside the home directory. Using these to look for what parameters the different algorithms will take for fitting and predicting will be very helpful.
 4. `make ui` - Run the command line interface that allows for simple data manipulation and applying a machine learning algorithm for people not adept at OCaml programming.
 
 ## Usage and demos

Several demos in the form of prewritten Jupyter notebooks are offered, the following list contains some of the most useful ones:

 1. `dataframe.ipynb` - Data loading and manipulation examples
 2. `matrix.ipynb` - Matrix operations examples
 3.  `statistics.ipynb` - Statistics operations examples 
 4.  `<machine_learning_algo>.ipynb` - Examples on the usage of different machine learning algorithms is showcased, along with a visual representation generated using the `Archimedes` library.
