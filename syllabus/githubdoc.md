Github Document
================

> ### Learning Objectives
>
> -   Define a function that takes arguments
> -   Return a value from a function
> -   Test a function
> -   Set default values for function arguments

------------------------------------------------------------------------

Motivation
----------

-   R comes with many functions (and packages) that let us perform a wide variety of tasks.
-   Most of the things we do in R is via calling some function.
-   Sometimes, however, there's no function to do what we want to achieve.
-   Now we want to write functions ourselves
-   Idea: avoid repetitive coding (errors will creep in)

### Anatomy of a function

So far you've been using a number of functions in R. Now it is time to see how you can create and use your own functions.

To define a new function in R you use the function `function()`. You need to specify a name for the function, and then assign `function()` to the chosen name. You also need to define optional arguments (i.e. inputs). And of course, you must write the code (i.e. the body) so the function does something when you use it:

``` r
# anatomy of a function
some_name <- function(arguments) {
  # body of the function
}
```

-   Generally, you give a name to a function.
-   A function takes one or more inputs (or none), known as *arguments*.
-   The expressions forming the operations comprise the **body** of the function.
-   Usually, you wrap the body of the functions with curly braces.
-   A function returns a single value.

A less abstract function could have the following structure:

``` r
function_name <- function(arg1, arg2, etc) 
{
  expression_1
  expression_2
  ...
  expression_n
}
```

### From Fahrenheit to Celsius

Let's consider a typical programming example that involves converting fahrenheit degrees into celsius degrees. The conversion formula is (*F* − 32)×5/9 = *C*. Here's some R code to convert 100 fahrenheit degrees into Celsius degrees:

``` r
# fahrenheit degrees
far_deg <- 100

# convert to celsius
(far_deg - 32) * (5/9)
```

    ## [1] 37.77778

What if you want to conver 90 fahrenheit degrees in Celsius degrees? One option would be to rewrite the previous lines as:

``` r
# fahrenheit degrees
far_deg <- 90

# convert to celsius
(far_deg - 32) * (5/9)
```

    ## [1] 32.22222

However, retyping many lines of code can be very boring, tedious, and inefficient. To make your code reusable in a more efficient manner, you will have to write functions.