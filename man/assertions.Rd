% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R, R/generated_assertions.R
\name{assertions}
\alias{assertions}
\alias{assert_dir_exists}
\alias{assert_file_exists}
\alias{assert_is_function}
\alias{assert_is_one_of}
\alias{assert_has_class}
\alias{assert_has_one_of_classes}
\alias{assert_is_number}
\alias{assert_is_character}
\alias{assert_is_double}
\alias{assert_is_integer}
\alias{assert_is_logical}
\alias{assert_is_Date}
\alias{assert_is_factor}
\alias{assert_is_factor_with_levels}
\alias{assert_is_atom}
\alias{assert_is_vector}
\alias{assert_is_matrix}
\alias{assert_is_data.frame}
\alias{assert_is_data.table}
\alias{assert_is_nonNA}
\alias{assert_is_NULL}
\alias{assert_is_list}
\alias{assert_is_named_list}
\alias{assert_is_uniquely_named_list}
\alias{assert_is_data_table}
\alias{assert_is_call}
\alias{assert_is_name}
\alias{assert_is_expression}
\alias{assert_is_language_object}
\alias{assert_has_names}
\alias{assert_is_named}
\alias{assert_is_uniquely_named}
\alias{assert_atom_is_in_set}
\alias{assert_vector_elems_are_in_set}
\alias{assert_has_only_names}
\alias{assert_is_data.table_with_required_names}
\alias{assert_is_data_table_with_required_names}
\alias{assert_has_length}
\alias{assert_is_between_inclusive}
\alias{assert_is_between_exclusive}
\alias{assert_is_gte}
\alias{assert_is_gt}
\alias{assert_is_lte}
\alias{assert_is_lt}
\alias{assert_is_ltezero}
\alias{assert_is_ltzero}
\alias{assert_is_gtezero}
\alias{assert_is_gtzero}
\alias{assert_is_Date_atom}
\alias{assert_is_Date_matrix}
\alias{assert_is_Date_vector}
\alias{assert_is_Date_nonNA_atom}
\alias{assert_is_Date_nonNA_matrix}
\alias{assert_is_Date_nonNA_vector}
\alias{assert_is_character_atom}
\alias{assert_is_character_matrix}
\alias{assert_is_character_vector}
\alias{assert_is_character_nonNA_atom}
\alias{assert_is_character_nonNA_matrix}
\alias{assert_is_character_nonNA_vector}
\alias{assert_is_double_atom}
\alias{assert_is_double_matrix}
\alias{assert_is_double_vector}
\alias{assert_is_double_gtezero_atom}
\alias{assert_is_double_gtezero_matrix}
\alias{assert_is_double_gtezero_vector}
\alias{assert_is_double_gtzero_atom}
\alias{assert_is_double_gtzero_matrix}
\alias{assert_is_double_gtzero_vector}
\alias{assert_is_double_ltezero_atom}
\alias{assert_is_double_ltezero_matrix}
\alias{assert_is_double_ltezero_vector}
\alias{assert_is_double_ltzero_atom}
\alias{assert_is_double_ltzero_matrix}
\alias{assert_is_double_ltzero_vector}
\alias{assert_is_double_nonNA_atom}
\alias{assert_is_double_nonNA_matrix}
\alias{assert_is_double_nonNA_vector}
\alias{assert_is_double_nonNA_gtezero_atom}
\alias{assert_is_double_nonNA_gtezero_matrix}
\alias{assert_is_double_nonNA_gtezero_vector}
\alias{assert_is_double_nonNA_gtzero_atom}
\alias{assert_is_double_nonNA_gtzero_matrix}
\alias{assert_is_double_nonNA_gtzero_vector}
\alias{assert_is_double_nonNA_ltezero_atom}
\alias{assert_is_double_nonNA_ltezero_matrix}
\alias{assert_is_double_nonNA_ltezero_vector}
\alias{assert_is_double_nonNA_ltzero_atom}
\alias{assert_is_double_nonNA_ltzero_matrix}
\alias{assert_is_double_nonNA_ltzero_vector}
\alias{assert_is_factor_atom}
\alias{assert_is_factor_matrix}
\alias{assert_is_factor_vector}
\alias{assert_is_factor_nonNA_atom}
\alias{assert_is_factor_nonNA_matrix}
\alias{assert_is_factor_nonNA_vector}
\alias{assert_is_integer_atom}
\alias{assert_is_integer_matrix}
\alias{assert_is_integer_vector}
\alias{assert_is_integer_gtezero_atom}
\alias{assert_is_integer_gtezero_matrix}
\alias{assert_is_integer_gtezero_vector}
\alias{assert_is_integer_gtzero_atom}
\alias{assert_is_integer_gtzero_matrix}
\alias{assert_is_integer_gtzero_vector}
\alias{assert_is_integer_ltezero_atom}
\alias{assert_is_integer_ltezero_matrix}
\alias{assert_is_integer_ltezero_vector}
\alias{assert_is_integer_ltzero_atom}
\alias{assert_is_integer_ltzero_matrix}
\alias{assert_is_integer_ltzero_vector}
\alias{assert_is_integer_nonNA_atom}
\alias{assert_is_integer_nonNA_matrix}
\alias{assert_is_integer_nonNA_vector}
\alias{assert_is_integer_nonNA_gtezero_atom}
\alias{assert_is_integer_nonNA_gtezero_matrix}
\alias{assert_is_integer_nonNA_gtezero_vector}
\alias{assert_is_integer_nonNA_gtzero_atom}
\alias{assert_is_integer_nonNA_gtzero_matrix}
\alias{assert_is_integer_nonNA_gtzero_vector}
\alias{assert_is_integer_nonNA_ltezero_atom}
\alias{assert_is_integer_nonNA_ltezero_matrix}
\alias{assert_is_integer_nonNA_ltezero_vector}
\alias{assert_is_integer_nonNA_ltzero_atom}
\alias{assert_is_integer_nonNA_ltzero_matrix}
\alias{assert_is_integer_nonNA_ltzero_vector}
\alias{assert_is_logical_atom}
\alias{assert_is_logical_matrix}
\alias{assert_is_logical_vector}
\alias{assert_is_logical_nonNA_atom}
\alias{assert_is_logical_nonNA_matrix}
\alias{assert_is_logical_nonNA_vector}
\alias{assert_is_number_atom}
\alias{assert_is_number_matrix}
\alias{assert_is_number_vector}
\alias{assert_is_number_gtezero_atom}
\alias{assert_is_number_gtezero_matrix}
\alias{assert_is_number_gtezero_vector}
\alias{assert_is_number_gtzero_atom}
\alias{assert_is_number_gtzero_matrix}
\alias{assert_is_number_gtzero_vector}
\alias{assert_is_number_ltezero_atom}
\alias{assert_is_number_ltezero_matrix}
\alias{assert_is_number_ltezero_vector}
\alias{assert_is_number_ltzero_atom}
\alias{assert_is_number_ltzero_matrix}
\alias{assert_is_number_ltzero_vector}
\alias{assert_is_number_nonNA_atom}
\alias{assert_is_number_nonNA_matrix}
\alias{assert_is_number_nonNA_vector}
\alias{assert_is_number_nonNA_gtezero_atom}
\alias{assert_is_number_nonNA_gtezero_matrix}
\alias{assert_is_number_nonNA_gtezero_vector}
\alias{assert_is_number_nonNA_gtzero_atom}
\alias{assert_is_number_nonNA_gtzero_matrix}
\alias{assert_is_number_nonNA_gtzero_vector}
\alias{assert_is_number_nonNA_ltezero_atom}
\alias{assert_is_number_nonNA_ltezero_matrix}
\alias{assert_is_number_nonNA_ltezero_vector}
\alias{assert_is_number_nonNA_ltzero_atom}
\alias{assert_is_number_nonNA_ltzero_matrix}
\alias{assert_is_number_nonNA_ltzero_vector}
\title{Assertions}
\usage{
assert_dir_exists(x, x_nm = NULL)

assert_file_exists(x, x_nm = NULL)

assert_is_function(x, x_nm = NULL)

assert_is_one_of(x, x_nm = NULL, fun_nms, funs_arg_list = NULL)

assert_has_class(x, x_nm = NULL, required_class)

assert_has_one_of_classes(x, x_nm = NULL, classes)

assert_is_number(x, x_nm = NULL)

assert_is_character(x, x_nm = NULL)

assert_is_double(x, x_nm = NULL)

assert_is_integer(x, x_nm = NULL)

assert_is_logical(x, x_nm = NULL)

assert_is_Date(x, x_nm = NULL)

assert_is_factor(x, x_nm = NULL)

assert_is_factor_with_levels(x, x_nm = NULL, expected_levels)

assert_is_atom(x, x_nm = NULL)

assert_is_vector(x, x_nm = NULL)

assert_is_matrix(x, x_nm = NULL)

assert_is_data.frame(x, x_nm = NULL)

assert_is_data.table(x, x_nm = NULL)

assert_is_nonNA(x, x_nm = NULL)

assert_is_NULL(x, x_nm = NULL)

assert_is_list(x, x_nm = NULL)

assert_is_named_list(x, x_nm = NULL)

assert_is_uniquely_named_list(x, x_nm = NULL)

assert_is_data_table(x, x_nm = NULL)

assert_is_call(x, x_nm = NULL)

assert_is_name(x, x_nm = NULL)

assert_is_expression(x, x_nm = NULL)

assert_is_language_object(x, x_nm = NULL)

assert_has_names(x, x_nm = NULL, required_names)

assert_is_named(x, x_nm = NULL)

assert_is_uniquely_named(x, x_nm = NULL)

assert_atom_is_in_set(x, x_nm = NULL, set)

assert_vector_elems_are_in_set(x, x_nm = NULL, set)

assert_has_only_names(x, x_nm = NULL, required_names)

assert_is_data.table_with_required_names(x, x_nm = NULL, required_names)

assert_is_data_table_with_required_names(x, x_nm = NULL, required_names)

assert_has_length(x, x_nm = NULL, required_length)

assert_is_between_inclusive(x, x_nm = NULL, lo, hi)

assert_is_between_exclusive(x, x_nm = NULL, lo, hi)

assert_is_gte(x, x_nm = NULL, lo)

assert_is_gt(x, x_nm = NULL, lo)

assert_is_lte(x, x_nm = NULL, hi)

assert_is_lt(x, x_nm = NULL, hi)

assert_is_ltezero(x, x_nm = NULL)

assert_is_ltzero(x, x_nm = NULL)

assert_is_gtezero(x, x_nm = NULL)

assert_is_gtzero(x, x_nm = NULL)

assert_is_Date_atom(x, x_nm = NULL)

assert_is_Date_matrix(x, x_nm = NULL)

assert_is_Date_vector(x, x_nm = NULL)

assert_is_Date_nonNA_atom(x, x_nm = NULL)

assert_is_Date_nonNA_matrix(x, x_nm = NULL)

assert_is_Date_nonNA_vector(x, x_nm = NULL)

assert_is_character_atom(x, x_nm = NULL)

assert_is_character_matrix(x, x_nm = NULL)

assert_is_character_vector(x, x_nm = NULL)

assert_is_character_nonNA_atom(x, x_nm = NULL)

assert_is_character_nonNA_matrix(x, x_nm = NULL)

assert_is_character_nonNA_vector(x, x_nm = NULL)

assert_is_double_atom(x, x_nm = NULL)

assert_is_double_matrix(x, x_nm = NULL)

assert_is_double_vector(x, x_nm = NULL)

assert_is_double_gtezero_atom(x, x_nm = NULL)

assert_is_double_gtezero_matrix(x, x_nm = NULL)

assert_is_double_gtezero_vector(x, x_nm = NULL)

assert_is_double_gtzero_atom(x, x_nm = NULL)

assert_is_double_gtzero_matrix(x, x_nm = NULL)

assert_is_double_gtzero_vector(x, x_nm = NULL)

assert_is_double_ltezero_atom(x, x_nm = NULL)

assert_is_double_ltezero_matrix(x, x_nm = NULL)

assert_is_double_ltezero_vector(x, x_nm = NULL)

assert_is_double_ltzero_atom(x, x_nm = NULL)

assert_is_double_ltzero_matrix(x, x_nm = NULL)

assert_is_double_ltzero_vector(x, x_nm = NULL)

assert_is_double_nonNA_atom(x, x_nm = NULL)

assert_is_double_nonNA_matrix(x, x_nm = NULL)

assert_is_double_nonNA_vector(x, x_nm = NULL)

assert_is_double_nonNA_gtezero_atom(x, x_nm = NULL)

assert_is_double_nonNA_gtezero_matrix(x, x_nm = NULL)

assert_is_double_nonNA_gtezero_vector(x, x_nm = NULL)

assert_is_double_nonNA_gtzero_atom(x, x_nm = NULL)

assert_is_double_nonNA_gtzero_matrix(x, x_nm = NULL)

assert_is_double_nonNA_gtzero_vector(x, x_nm = NULL)

assert_is_double_nonNA_ltezero_atom(x, x_nm = NULL)

assert_is_double_nonNA_ltezero_matrix(x, x_nm = NULL)

assert_is_double_nonNA_ltezero_vector(x, x_nm = NULL)

assert_is_double_nonNA_ltzero_atom(x, x_nm = NULL)

assert_is_double_nonNA_ltzero_matrix(x, x_nm = NULL)

assert_is_double_nonNA_ltzero_vector(x, x_nm = NULL)

assert_is_factor_atom(x, x_nm = NULL)

assert_is_factor_matrix(x, x_nm = NULL)

assert_is_factor_vector(x, x_nm = NULL)

assert_is_factor_nonNA_atom(x, x_nm = NULL)

assert_is_factor_nonNA_matrix(x, x_nm = NULL)

assert_is_factor_nonNA_vector(x, x_nm = NULL)

assert_is_integer_atom(x, x_nm = NULL)

assert_is_integer_matrix(x, x_nm = NULL)

assert_is_integer_vector(x, x_nm = NULL)

assert_is_integer_gtezero_atom(x, x_nm = NULL)

assert_is_integer_gtezero_matrix(x, x_nm = NULL)

assert_is_integer_gtezero_vector(x, x_nm = NULL)

assert_is_integer_gtzero_atom(x, x_nm = NULL)

assert_is_integer_gtzero_matrix(x, x_nm = NULL)

assert_is_integer_gtzero_vector(x, x_nm = NULL)

assert_is_integer_ltezero_atom(x, x_nm = NULL)

assert_is_integer_ltezero_matrix(x, x_nm = NULL)

assert_is_integer_ltezero_vector(x, x_nm = NULL)

assert_is_integer_ltzero_atom(x, x_nm = NULL)

assert_is_integer_ltzero_matrix(x, x_nm = NULL)

assert_is_integer_ltzero_vector(x, x_nm = NULL)

assert_is_integer_nonNA_atom(x, x_nm = NULL)

assert_is_integer_nonNA_matrix(x, x_nm = NULL)

assert_is_integer_nonNA_vector(x, x_nm = NULL)

assert_is_integer_nonNA_gtezero_atom(x, x_nm = NULL)

assert_is_integer_nonNA_gtezero_matrix(x, x_nm = NULL)

assert_is_integer_nonNA_gtezero_vector(x, x_nm = NULL)

assert_is_integer_nonNA_gtzero_atom(x, x_nm = NULL)

assert_is_integer_nonNA_gtzero_matrix(x, x_nm = NULL)

assert_is_integer_nonNA_gtzero_vector(x, x_nm = NULL)

assert_is_integer_nonNA_ltezero_atom(x, x_nm = NULL)

assert_is_integer_nonNA_ltezero_matrix(x, x_nm = NULL)

assert_is_integer_nonNA_ltezero_vector(x, x_nm = NULL)

assert_is_integer_nonNA_ltzero_atom(x, x_nm = NULL)

assert_is_integer_nonNA_ltzero_matrix(x, x_nm = NULL)

assert_is_integer_nonNA_ltzero_vector(x, x_nm = NULL)

assert_is_logical_atom(x, x_nm = NULL)

assert_is_logical_matrix(x, x_nm = NULL)

assert_is_logical_vector(x, x_nm = NULL)

assert_is_logical_nonNA_atom(x, x_nm = NULL)

assert_is_logical_nonNA_matrix(x, x_nm = NULL)

assert_is_logical_nonNA_vector(x, x_nm = NULL)

assert_is_number_atom(x, x_nm = NULL)

assert_is_number_matrix(x, x_nm = NULL)

assert_is_number_vector(x, x_nm = NULL)

assert_is_number_gtezero_atom(x, x_nm = NULL)

assert_is_number_gtezero_matrix(x, x_nm = NULL)

assert_is_number_gtezero_vector(x, x_nm = NULL)

assert_is_number_gtzero_atom(x, x_nm = NULL)

assert_is_number_gtzero_matrix(x, x_nm = NULL)

assert_is_number_gtzero_vector(x, x_nm = NULL)

assert_is_number_ltezero_atom(x, x_nm = NULL)

assert_is_number_ltezero_matrix(x, x_nm = NULL)

assert_is_number_ltezero_vector(x, x_nm = NULL)

assert_is_number_ltzero_atom(x, x_nm = NULL)

assert_is_number_ltzero_matrix(x, x_nm = NULL)

assert_is_number_ltzero_vector(x, x_nm = NULL)

assert_is_number_nonNA_atom(x, x_nm = NULL)

assert_is_number_nonNA_matrix(x, x_nm = NULL)

assert_is_number_nonNA_vector(x, x_nm = NULL)

assert_is_number_nonNA_gtezero_atom(x, x_nm = NULL)

assert_is_number_nonNA_gtezero_matrix(x, x_nm = NULL)

assert_is_number_nonNA_gtezero_vector(x, x_nm = NULL)

assert_is_number_nonNA_gtzero_atom(x, x_nm = NULL)

assert_is_number_nonNA_gtzero_matrix(x, x_nm = NULL)

assert_is_number_nonNA_gtzero_vector(x, x_nm = NULL)

assert_is_number_nonNA_ltezero_atom(x, x_nm = NULL)

assert_is_number_nonNA_ltezero_matrix(x, x_nm = NULL)

assert_is_number_nonNA_ltezero_vector(x, x_nm = NULL)

assert_is_number_nonNA_ltzero_atom(x, x_nm = NULL)

assert_is_number_nonNA_ltzero_matrix(x, x_nm = NULL)

assert_is_number_nonNA_ltzero_vector(x, x_nm = NULL)
}
\arguments{
\item{x}{R object to be tested (mandatory, no default)}

\item{x_nm}{\verb{[NULL, character]} (optional, default \code{NULL})

the name of the object \code{x} to mention in possible error message.
\itemize{
\item \code{NULL}: taken as \code{deparse(substitute(x))}
\item \code{character}: the name as a string
}}

\item{fun_nms}{\verb{[character]} (mandatory, no default)

names of assertion funs to run}

\item{funs_arg_list}{\verb{[list, NULL]} (optional, default \code{NULL})
\itemize{
\item \code{NULL}: no additional arguments are passed to functions specified in
\code{fun_nms}
\item \code{list}: named elements of this list are matched to the names of arguments
of each function specified in \code{fun_nms}, and matching arguments are
used; e.g. if one fun has arg \code{y} and other has \code{z}, you may pass
\code{list(y = my_y, z = my_z)}.
}}

\item{required_class}{\verb{[character]} (mandatory, no default)

string specifying class that \code{x} MUST have}

\item{classes}{\verb{[character]} (mandatory, no default)

character vector specifying classes of which \code{x} MUST have at least one}

\item{expected_levels}{\verb{[character]} (mandatory, no default)

exact character string vector of levels that \code{x} must have; i.e.
\code{levels(x)} and \code{expected_levels} must be the same set and they must be
in the same order}

\item{required_names}{\verb{[character]} (mandatory, no default)
column names that \code{x} MUST have}

\item{set}{\verb{[vector]} (mandatory, no default)

vector (set, but uniqueness is not asserted) of allowed values}

\item{required_length}{\verb{[integer]} (mandatory, no default)
\code{x} MUST be of this length}

\item{lo}{\verb{[number]} (mandatory, no default)
lower bound for \code{x}}

\item{hi}{\verb{[number]} (mandatory, no default)
upper bound for \code{x}}
}
\description{
Collection of assertion functions which raise an error if the test does not
pass.
}
