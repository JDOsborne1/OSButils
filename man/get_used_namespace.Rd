% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/namespace_utils.R
\name{get_used_namespace}
\alias{get_used_namespace}
\title{Get the currently used namespace}
\usage{
get_used_namespace(.function_name)
}
\arguments{
\item{.function_name}{The function name to get the formal name for.}
}
\value{

}
\description{
Function to get the used namespace and prepend it to the
  function name.

  This has the effect of replacing a provided function name, with the formal
  name which replicates the behaviour as it is when called in the current
  environment. This is designed to make conversions from conventional code to
  a package format easier.

  When it is used on a function with no defintion, or a function which is a
  part of the base packages, it will simply return the input unchanged.
}
