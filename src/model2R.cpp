#include <ggdmcHeaders/common_type_casting.h>
// #include <ggdmcHeaders/design.h> // model_util.h included optional
#include <ggdmcHeaders/likelihood_type_casting.h>
// model_type_casting.h>

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
    std::string model_str = get_model_type(model_r);

    // Set row and column names
    auto rownames =
        get_core_parameters(d_ptr->m_core_parameter_names, model_str);
    auto colnames = d_ptr->m_accumulator_names;

    // for (const auto &item : colnames)
    // {
    //     Rcpp::Rcout << "accumulator names " << item << "\n";
    // }

    Rcpp::List dimnames = Rcpp::List::create(rownames, colnames);

    Rcpp::List out(d_ptr->m_n_cell);
    Rcpp::Rcout << "Cell (ncell =  " << d_ptr->m_n_cell << "):\n";

    for (size_t i = 0; i < d_ptr->m_n_cell; ++i)
    {
        Rcpp::Rcout << d_ptr->m_cell_names[i] << std::endl;
        d_ptr->set_parameter_values(i, parameters);

        Rcpp::NumericMatrix parameter_matrix =
            std_mat_to_NumericMatrix(d_ptr->m_parameter_matrix[i]);
        parameter_matrix.attr("dimnames") = dimnames;
        out[i] = parameter_matrix;
    }
    d_ptr->m_parameter_matrix_arma.print("Parameter matrix");

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

inline bool all_empty_or_whitespace(const strVec &v)
{
    if (v.empty())
        return true;

    for (const auto &s : v)
    {
        if (!s.empty() && s.find_first_not_of(" \t\n\r") != std::string::npos)
        {
            return false; // found something non-empty
        }
    }
    return true;
}

/* ---------- External interface--------------*/
//' @rdname build_cell_names_r
//' @export
// [[Rcpp::export]]
Rcpp::List build_cell_names_r(
    const Rcpp::List &parameter_map_r,
    const Rcpp::Nullable<Rcpp::List> &factors_r = R_NilValue,
    const Rcpp::Nullable<Rcpp::CharacterVector> accumulators_r = R_NilValue)
{
    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    strVec cell_names;
    strVec sorted_factors;

    std::map<std::string, strVec> factors;
    strVec accumulators;

    if (factors_r.isNotNull())
    {
        Rcpp::List factors_list = factors_r.get();
        factors = list_to_map<std::string>(factors_list);
    }

    if (accumulators_r.isNotNull() && factors_r.isNotNull())
    {
        // else if (accumulators_r.isNotNull() && factors_r.isNull())
        Rcpp::CharacterVector accumulators_vec = accumulators_r.get();
        for (int i = 0; i < accumulators_vec.size(); i++)
        {
            accumulators.push_back(Rcpp::as<std::string>(accumulators_vec[i]));
        }

        if (accumulators.empty())
        {
            Rcpp::stop("Please set 'accumulators' to NULL when it is empty.");
        }
        auto res = build_cell_names(parameter_map, factors, accumulators);
        cell_names = std::move(res.first);
        sorted_factors = std::move(res.second);
    }
    else if (accumulators_r.isNull() && factors_r.isNotNull())
    {
        // Rcpp::Rcout << "build_cell_names_no_accumulator\n";
        // auto factors = list_to_map<std::string>(factors_r);
        auto res = build_cell_names_no_accumulator(parameter_map, factors);
        cell_names = std::move(res.first);
        sorted_factors = std::move(res.second);
    }
    else
    {
        cell_names.clear();
        cell_names.push_back("Cell");
        sorted_factors.clear();
        sorted_factors.push_back("Factor");
    }

    // ---- Patch: replace with "Cell" if empty ----
    if (all_empty_or_whitespace(cell_names))
    {
        cell_names.clear();
        cell_names.push_back("Cell");
    }

    Rcpp::List out(2);
    out[0] = Rcpp::wrap(cell_names); // cell names
    out[1] = Rcpp::wrap(sorted_factors);
    return out;
}

//' @rdname build_model_boolean_r
//' @export
// [[Rcpp::export]]
Rcpp::LogicalVector build_model_boolean_r(
    const Rcpp::List &parameter_map_r,
    const Rcpp::Nullable<Rcpp::List> &factors_r = R_NilValue,
    Rcpp::Nullable<Rcpp::CharacterVector> accumulators_r = R_NilValue,
    Rcpp::Nullable<Rcpp::List> match_map_r = R_NilValue)
{
    // ---- Convert maps ----
    std::map<std::string, strVec> parameter_map =
        list_to_map<std::string>(parameter_map_r);

    std::map<std::string, strVec> factors;
    if (factors_r.isNotNull())
    {
        Rcpp::List factors_list = factors_r.get();
        factors = list_to_map<std::string>(factors_list);
    }

    // ---- Accumulators: nullable -> pointer + presence flag ----
    strVec accumulators_vec;
    const strVec *accumulators_ptr = nullptr;
    if (accumulators_r.isNotNull())
    {
        Rcpp::CharacterVector rr(accumulators_r);
        accumulators_vec = Rcpp::as<strVec>(rr);
        if (!accumulators_vec.empty())
            accumulators_ptr = &accumulators_vec;
    }
    const bool has_accumulators = (accumulators_ptr != nullptr);

    // ---- match_map: nullable -> pointer + presence flag ----
    Rcpp::List match_map_tmp;
    const Rcpp::List *match_map_ptr = nullptr;
    if (match_map_r.isNotNull())
    {
        match_map_tmp = Rcpp::List(match_map_r);
        if (match_map_tmp.size() > 0)
            match_map_ptr = &match_map_tmp;
    }
    const bool has_match_map = (match_map_ptr != nullptr);

    // ---- Branch: 3D (accu + match_map) vs 2D (no accu, no match) ----
    if (has_accumulators && has_match_map)
    {
        std::map<std::string, std::map<std::string, std::string>> match_map =
            nested_list_to_map(*match_map_ptr); // callee expects a reference

        // core builds [cell][parameter][accumulator]
        std::vector<std::vector<std::vector<bool>>> cpp_out =
            build_model_boolean(parameter_map, factors, *accumulators_ptr,
                                match_map);

        return std_ucube_to_R_ucube(cpp_out);
    }
    else if (!has_accumulators && !has_match_map)
    {
        // 3D array with only 1 element in the third dimension
        std::vector<std::vector<std::vector<bool>>> cpp_out =
            build_model_boolean_noaccu_nomatch(parameter_map, factors);
        return std_ucube_to_R_ucube(cpp_out);
    }
    else
    {
        // Mixed/unsupported combination:
        // - accumulators given but no match_map
        // - match_map given but no accumulators
        // Decide policy; here we error to keep semantics unambiguous.
        Rcpp::stop("Inconsistent inputs: accumulators and match_map must be "
                   "both present "
                   "for accumulator models, or both NULL/absent for "
                   "non-accumulator models.");
    }
}

//' @export
// [[Rcpp::export]]
std::vector<std::string> bind_condition2parameters_r(
    const Rcpp::List &parameter_map_r,
    const Rcpp::Nullable<Rcpp::List> &factors_r = R_NilValue)
{
    std::map<std::string, strVec> factors;

    if (factors_r.isNotNull())
    {
        Rcpp::List factors_list = factors_r.get();
        factors = list_to_map<std::string>(factors_list);
    }

    auto parameter_map = list_to_map<std::string>(parameter_map_r);
    return add_M(parameter_map, factors);
}

//' Get Index Mapping for the Node 1 Accumulator
//'
//' @description
//' Generates an integer matrix mapping experimental design cells to their
//' corresponding indexes of the node 1 accumulator. The node 1 accumulator
//' is the theoretical accumulator that reaches the threshold first. This
//' function is primarily used for the LBA model.
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
//'   \item Computes node indices for each condition-response pair
//'   \item Returns results as an R-compatible integer matrix
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

//' @rdname get_pnames
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
