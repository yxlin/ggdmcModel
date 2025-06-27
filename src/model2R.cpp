#include <ggdmcHeaders/common_type_casting.h>
#include <ggdmcHeaders/design.h>
#include <ggdmcHeaders/model_type_casting.h>

/* ----------Test functions (removed to reduce the size)--------------*/

//' @rdname bind_condition2parameters_r
//' @export
// [[Rcpp::export]]
Rcpp::List
split_parameter_x_condition(const Rcpp::CharacterVector &parameter_M_r)
{
    auto parameters = Rcpp::as<std::vector<std::string>>(parameter_M_r);
    auto result = split_parameter_condition(parameters); // in model_utils

    Rcpp::List out;
    for (size_t i = 0; i < result.size(); ++i)
    {
        out.push_back(Rcpp::wrap(result[i]));
    }

    return out;
}

//' @rdname parameter_mapping_functions
//' @export
// [[Rcpp::export]]
std::vector<bool>
is_core_parameter_x_condition(const Rcpp::List &parameter_map_r,
                              const Rcpp::List &factors_r)
{
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);

    // model_utils
    return is_core_parameter_x_condition(parameter_map, factors);
}

//' @rdname parameter_mapping_functions
//' @export
// [[Rcpp::export]]
std::vector<bool> is_parameter_x_condition(const Rcpp::List &parameter_map_r,
                                           const Rcpp::List &factors_r)
{

    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);
    auto parameter_x_condition = add_M(parameter_map, factors);

    // model_utils
    return is_parameter_condition_associated(parameter_map,
                                             parameter_x_condition, factors);
}

Rcpp::List map_to_named_list(std::map<std::string, std::string> factor_cells)
{
    Rcpp::List result(factor_cells.size());
    Rcpp::CharacterVector names(factor_cells.size());

    int i = 0;
    for (const auto &pair : factor_cells)
    {
        names[i] = pair.first;
        result[i] = pair.second;
        i++;
    }

    result.attr("names") = names;
    return result;
}

//' @rdname parameter_mapping_functions
//' @export
// [[Rcpp::export]]
std::vector<std::string>
get_stimulus_level_r(const Rcpp::List &parameter_map_r,
                     const Rcpp::List &factors_r,
                     const std::vector<std::string> &accumulators_r)
{
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);

    auto [cell_names, factor_names] =
        build_cell_names(parameter_map, factors, accumulators_r);
    size_t n_cell = cell_names.size();

    std::vector<std::string> stimulus_levels(n_cell);
    Rcpp::List factor_cells(n_cell);

    for (size_t cell_idx = 0; cell_idx < n_cell; ++cell_idx)
    {

        stimulus_levels[cell_idx] = get_stimulus_level(cell_names[cell_idx]);
        std::map<std::string, std::string> factor_cell =
            get_factor_cells(cell_names[cell_idx], factor_names);
        factor_cells[cell_idx] = map_to_named_list(factor_cell);
    }

    return stimulus_levels;
}

//' @rdname parameter_mapping_functions
//' @export
// [[Rcpp::export]]
Rcpp::List get_factor_cells_r(const Rcpp::List &parameter_map_r,
                              const Rcpp::List &factors_r,
                              const std::vector<std::string> &accumulators_r)
{
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);

    auto [cell_names, factor_names] =
        build_cell_names(parameter_map, factors, accumulators_r);
    size_t n_cell = cell_names.size();

    Rcpp::List factor_cells(n_cell);

    for (size_t cell_idx = 0; cell_idx < n_cell; ++cell_idx)
    {
        std::map<std::string, std::string> factor_cell =
            get_factor_cells(cell_names[cell_idx], factor_names);
        factor_cells[cell_idx] = map_to_named_list(factor_cell);
    }

    return factor_cells;
}

std::vector<std::string>
get_core_parameters(const std::vector<std::string> &input,
                    const std::string model_str)
{
    std::vector<std::string> out(input.size());
    for (size_t i = 0; i < input.size(); ++i)
    {

        if (input[i] == "B" && model_str == "lba")
        {
            out[i] = "b";
        }
        else
        {
            out[i] = input[i];
        }
    }
    return out;
}

Rcpp::NumericMatrix
std_mat_to_NumericMatrix(const std::vector<std::vector<double>> &input)
{
    // Check if all inner vectors have the same size
    if (input.empty())
    {
        return Rcpp::NumericMatrix(0);
    }
    size_t nrows = input.size();
    size_t ncols = input[0].size();

    for (size_t i = 1; i < nrows; ++i)
    {
        if (input[i].size() != ncols)
        {
            Rcpp::stop("All inner vectors must have the same length");
        }
    }

    Rcpp::NumericMatrix out(nrows, ncols);

    for (size_t i = 0; i < nrows; ++i)
    {
        for (size_t j = 0; j < ncols; ++j)
        {
            out(i, j) = input[i][j];
        }
    }
    return out;
}

//' @rdname model_parameter_utils
//' @export
// [[Rcpp::export]]
Rcpp::List table_parameters(const Rcpp::S4 &model_r,
                            const Rcpp::NumericVector &parameters_r)
{
    auto d_ptr = new_design(model_r);
    auto parameters = Rcpp::as<std::vector<double>>(parameters_r);

    d_ptr->prepare_parameter_matrix();

    std::string model_str = model_r.slot("type");

    // Set row and column names
    auto rownames =
        get_core_parameters(d_ptr->m_core_parameter_names, model_str);
    auto colnames = d_ptr->m_accumulator_names;
    Rcpp::List dimnames = Rcpp::List::create(rownames, colnames);

    Rcpp::List out(d_ptr->m_n_cell);
    Rcpp::Rcout << "Cell (ncell =  " << d_ptr->m_n_cell << "):";
    for (size_t i = 0; i < d_ptr->m_n_cell; ++i)
    {
        Rcpp::Rcout << d_ptr->m_cell_names[i] << std::endl;
        d_ptr->set_parameter_values(i, parameters);

        Rcpp::NumericMatrix parameter_matrix =
            std_mat_to_NumericMatrix(d_ptr->m_parameter_matrix[i]);
        parameter_matrix.attr("dimnames") = dimnames;
        out[i] = parameter_matrix;
    }
    // d_ptr->m_parameter_matrix_arma.print("Parameter matrix");

    out.attr("names") = d_ptr->m_cell_names;
    return out;
}

//' @rdname model_parameter_utils
//' @export
// [[Rcpp::export]]
int print_parameter_map(const Rcpp::S4 &model_r)
{
    auto d_ptr = new_design(model_r);
    d_ptr->print_all_parameters("All parameters: ");
    d_ptr->print_core_parameters("Core parameters: ");
    d_ptr->print_free_parameters("Free parameters: ");
    d_ptr->print_constants("Constant values: ");

    d_ptr->print_parameter_map("Parameter map: ");

    Rcpp::Rcout << "Cell (ncell =  " << d_ptr->m_n_cell << "): ";
    for (size_t cell_idx = 0; cell_idx < d_ptr->m_n_cell; ++cell_idx)
    {
        Rcpp::Rcout << d_ptr->m_cell_names[cell_idx] << "\t";
    }
    Rcpp::Rcout << std::endl;
    return 0;
}

/* ---------- External interface--------------*/
//' Find All Possible Conditions
//'
//' @description
//' Constructs all possible condition combinations (i.e., cells)
//' based on experimental factors, parameter mappings, and response
//' definitions. Returns both cell names and sorted factor definitions.
//'
//' @param parameter_map_r An Rcpp::List where each element is a character
//' vector mapping parameters to conditions. Names should correspond to
//'        parameters.
//' @param factors_r An Rcpp::List where each element is a character vector of
//'        factor levels. Names should correspond to factor names.
//' @param responses_r A character vector (std::vector<std::string>) of response
//'        accumulator names.
//'
//' @return An Rcpp::List with two elements:
//' \itemize{
//'   \item \code{cell_names}: Character vector of all possible condition
//' combinations
//'   \item \code{sortedFactors}: The processed factor structure used to
//'   generate  cells
//' }
//'
//' @details
//' The function:
//' \enumerate{
//'   \item Converts R lists to C++ maps for efficient processing
//'   \item Generates all condition combinations via Cartesian product
//'   \item Handles special parameter mappings (like response accumulators)
//'   \item Returns both cell names and the factor structure used
//' }
//'
//' @section Typical Workflow:
//' This function is typically used to:
//' \enumerate{
//'   \item Establish the full experimental design space
//'   \item Verify factor/parameter compatibility
//'   \item Generate condition labels for model specification
//' }
//' This function primarily is to debug the internal process of model building.
//'
//' @examples
//' # A simple example
//' p_map <- list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1",
//'                st0 ="1")
//' factors <- list(S = c("s1", "s2"))
//' responses <- c("r1", "r2")
//' result <- build_cell_names_r(p_map, factors, responses)
//'
//' # cat("B (2 factors), t0, mean_v (3 factors), sd_v (2 factors)")
//' p_map <- list(
//'     A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
//'     sd_v = c("D", "M"), st0 = "1"
//' )
//' factors <- list(
//'     S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
//'     G = c("g1", "g2", "g3"), H = c("h1", "h2", "h3", "h4", "h5")
//' )
//' responses <- c("r1", "r2", "r3")
//' result <- build_cell_names_r(p_map, factors, responses)
//'
//' @export
// [[Rcpp::export]]
Rcpp::List build_cell_names_r(const Rcpp::List &parameter_map_r,
                              const Rcpp::List &factors_r,
                              const std::vector<std::string> &responses_r)
{
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);

    auto [cell_names, sortedFactors] =
        build_cell_names(parameter_map, factors, responses_r);

    Rcpp::List out(2);
    out[0] = cell_names;
    out[1] = factors;
    return out;
}

//' Build Model Boolean
//'
//' @description
//' Constructs a 3D boolean array indicating
//' parameter-condition-response association to represent the experimental
//' design.
//'
//' @param parameter_map_r An Rcpp::List where each element maps parameters
//'        to conditions (character vector). The element names indicates
//'        the model parameter. The element content is the factor name that
//'        assocaites with a model parameter.  \code{1} represents no
//'        assocation.
//' @param factors_r An Rcpp::List where each element defines factor levels
//'        (character vector). Names should be factor names.
//' @param accumulators_r A character vector (std::vector<std::string>)
//'        of accumulator names. I use `accumulator` to remind the
//'        difference of the implicit accumulator and the manifested
//'        response. Mostly, you may mix the two; however, sometimes,
//'        merging the two concepts may result in conceptual errors.
//' @param match_map_r An Rcpp::List that defines the mapping between
//' stimuli and responses, specifying which response are considered correct
//' or incorrect. (This is a nested list structure).
//'
//' @return An R logical array with dimensions:
//' \itemize{
//'   \item 1st dimension: Parameters (column)
//'   \item 2nd dimension: Conditions (row)
//'   \item 3rd dimension: Responses  (slice)
//' }
//' Where `TRUE` indicates the model assumes that a model parameter (1st
//' dimension) affects a condition (2nd dimension) at a particular response
//' (3rd dimension).
//'
//' @details
//' The function:
//' \enumerate{
//'   \item Converts all R inputs to C++ maps for efficient processing
//'   \item Builds experimental design cells using \code{build_cell_names}
//'   \item Processes parameter-condition mappings with \code{add_M}
//'   \item Applies match map constraints to determine valid combinations
//'   \item Returns results as a 3D logical array compatible with R
//' }
//'
//' @section Typical Use Case:
//' Used when you need to:
//' \itemize{
//'   \item Validate experimental design completeness
//'   \item Generate design matrices for model fitting
//'   \item Check response-condition constraints
//' }
//'
//' @examples
//' p_map <- list(A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1",
//'               t0 = "1")
//' match_map <- list(M = list(s1 = "r1", s2 = "r2"))
//' factors <- list(S = c("s1", "s2"))
//' accumulators <- c("r1", "r2")
//' result <- build_model_boolean_r(p_map, factors, accumulators, match_map)
//'
//' @export
// [[Rcpp::export]]
Rcpp::LogicalVector
build_model_boolean_r(const Rcpp::List &parameter_map_r,
                      const Rcpp::List &factors_r,
                      const std::vector<std::string> &accumulators_r,
                      const Rcpp::List &match_map_r)
{
    // arma::ucube
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);

    std::map<std::string, std::map<std::string, std::string>> match_map =
        nested_list_to_map(match_map_r);

    auto [cell_names, factor_names] =
        build_cell_names(parameter_map, factors, accumulators_r);

    auto parameter_x_condition_names = add_M(parameter_map, factors);

    // Rcpp::Rcout << "before build_model_boolean\n";
    std::vector<std::vector<std::vector<bool>>> cpp_out =
        build_model_boolean(parameter_map, factors, accumulators_r, match_map);

    return std_ucube_to_R_ucube(cpp_out);
}

//' Map Experimental Conditions to Model Parameters
//'
//' @description
//' Binds experimental conditions to model parameters by combining parameter
//' mappings and experimental factors, automatically handling the 'M' (
//' matching) factor.
//'
//' @param parameter_map_r A named list received from R (converted to
//' Rcpp::List) where:
//' \itemize{
//'   \item Names correspond to parameter names
//'   \item Elements are character vectors mapping conditions to parameter
//'}
//'
//' @param factors_r A named list of experimental factors where:
//' \itemize{
//'   \item Names are factor names
//'   \item Elements are character vectors of factor levels
//' }
//' @param parameter_M_r a string vector of parameter x condition.
//'
//' @return A character vector where each element represents a
//' parameter-condition binding in the format "parameter.condition".
//' The special 'M' factor is to represent matching and non-matching
//' true/false in the LBA model.
//'
//' @details
//' This function:
//' \enumerate{
//'   \item Converts R lists to C++ std::map containers for efficient lookup
//'   \item Processes the parameter mapping through `add_M()` to handle response
//' mappings
//'   \item Returns human-readable parameter-condition pairs  }
//'
//' @section C++ Implementation:
//' The function uses:
//' \itemize{
//'   \item Rcpp::List input for R compatibility
//'   \item std::map for efficient key-value lookups and name ordering
//'   \item Automatic conversion between R and C++ data structures
//' }
//'
//' @examples
//' p_map <- list(A = "1", B = "1", t0 = "1", mean_v = c("M", "S"), sd_v = "1",
//'           st0 = "1")
//' factors <- list(S = c("s1", "s2"))
//' parameter_M <-bind_condition2parameters_r(p_map, factors)
//' # [1] "A"               "B"               "mean_v.s1.false" "mean_v.s1.true"
//' # [5] "mean_v.s2.false" "mean_v.s2.true"  "sd_v"            "st0"
//' # [9] "t0"
//' result <- split_parameter_x_condition(parameter_M)
//' # [[1]]
//' # [1] "A"
//' #
//' # [[2]]
//' # [1] "B"
//' #
//' # [[3]]
//' # [1] "mean_v" "s1"     "false"
//' #
//' # [[4]]
//' # [1] "mean_v" "s1"     "true"
//' #
//' # [[5]]
//' # [1] "mean_v" "s2"     "false"
//' #
//' # [[6]]
//' # [1] "mean_v" "s2"     "true"
//' #
//' # [[7]]
//' # [1] "sd_v"
//' #
//' # [[8]]
//' # [1] "st0"
//' #
//' # [[9]]
//' # [1] "t0"
//' @export
// [[Rcpp::export]]
std::vector<std::string>
bind_condition2parameters_r(const Rcpp::List &parameter_map_r,
                            const Rcpp::List &factors_r)
{
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);
    return add_M(parameter_map, factors);
}

//' Get Node Index Mapping for First-Level Parameters
//'
//' @description
//' Generates an integer matrix mapping experimental design cells to their
//' corresponding first-level parameter indices, incorporating accumulator
//' (response) information.
//'
//' @param parameter_map_r An Rcpp::List where each element is a character
//' vector mapping parameters to conditions. Names should correspond to
//' parameters.
//' @param factors_r An Rcpp::List where each element is a character
//' vector of factor levels. Names should correspond to factor names.
//' @param accumulators_r A character vector of response accumulator names.
//'
//' @return An integer matrix with dimensions:
//' \itemize{
//'   \item Rows: Experimental conditions (cells)
//'   \item Columns: Accumulators (responses)
//' }
//' Where values represent parameter indices for each condition-response
//' combination.
//'
//' @details
//' The function:
//' \enumerate{
//'   \item Converts R lists to C++ maps for efficient processing
//'   \item Builds experimental design cells using \code{build_cell_names}
//'   \item Computes node indices for each condition-response pair
//'   \item Returns results as an R-compatible integer matrix
//' }
//'
//' @section Typical Use Case:
//' Primarily used for:
//' \itemize{
//'   \item Setting up hierarchical model structures
//'   \item Linking experimental conditions to parameter nodes
//'   \item Establishing response-specific parameter mappings
//' }
//'
//' @examples
//' cat("Flexible stimulus name")
//' p_map <- list(A = "1", B = "S", t0 = "E", mean_v = c("D", "M"),
//'                sd_v = "M", st0 = "1")
//' factors <- list(S = c("sti_1", "sti_2", "sti_3", "sti_4"),
//'                 D = c("d1", "d2"), E = c("e1", "e2"))
//' responses <- c("resp_1", "resp_2", "resp_3", "resp_4")
//'
//' # Get node indices
//' result <- get_node_1_index_r(p_map, factors, responses)
//' print(dim(result)[[1]])
//' # 64
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerMatrix
get_node_1_index_r(const Rcpp::List &parameter_map_r,
                   const Rcpp::List &factors_r,
                   const std::vector<std::string> &accumulators_r)
{
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    auto factors = list_to_map<std::string>(factors_r);

    auto [cell_names, factor_names] =
        build_cell_names(parameter_map, factors, accumulators_r);
    auto cpp_out = get_node_1_index(cell_names, accumulators_r);
    return std_umat_to_R_int_mat(cpp_out);
}

//' Get Free Parameter Names from Model
//'
//' @description
//' Extracts the names of free parameters from an S4 model object, with optional
//' debugging output to inspect both free and constant parameters.
//'
//' @param model_r An S4 object containing the model specification and design
//' @param debug Logical flag indicating whether to print debugging information
//'        about both free and fixed parameters (default: FALSE)
//'
//' @return A character vector of free parameter names in the model
//'
//' @details
//' The function:
//' \enumerate{
//'   \item Creates a new design object from the model
//'   \item Optionally prints debugging information about all parameters
//'   \item Returns only the names of free (non-constant) parameters
//' }
//'
//' @section Debugging Output:
//' When `debug = TRUE`, the function prints:
//' \itemize{
//'   \item Free parameters (those being estimated)
//'   \item Constants (fixed parameters)
//' }
//'
//' @examples
//' model <- BuildModel(
//'   p_map = list(A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1",
//'                t0 = "1"),
//'   match_map = list(M = list(s1 = "r1", s2 = "r2")),
//'   factors = list(S = c("s1", "s2")),
//'   constants = c(A = 0.75, mean_v.false = 1.5, sd_v = 1, st0 = 0),
//'   accumulators = c("r1", "r2"),
//'   type = "lba")
//'
//' pnames <- get_pnames(model)
//'
//' @export
// [[Rcpp::export]]
std::vector<std::string> get_pnames(const Rcpp::S4 &model_r, bool debug = false)
{
    auto d_ptr = new_design(model_r);
    if (debug)
    {
        d_ptr->print_free_parameters("Free parameters: ");
        d_ptr->print_constants("Constants: ");
    }
    return d_ptr->m_free_parameter_names;
}
