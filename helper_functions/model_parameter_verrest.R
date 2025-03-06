
model_parameter_verrest <- function(){
  data <- data.frame(
    Parameter = c("CL (L/day)", "Vc (L)", "Q (L/day)", "Vp (L)", "ka (hr⁻¹)", "F1",
                  "COVF,W1 (fractional change)", "COVF,CD (exponent of power relationship)",
                  "CL (CV%)", "COVF,W1 (CV%)", "Proportional error (CV%)"),
    Estimate = c(1.85, 13.6, 0.17, 2.22, 0.037, "1 (fixed)", -0.65, -2.40, 16.3, 74.8, 31.5),
    CI = c("1.75–1.94", "12.8–14.4", "0.13–0.21", "1.96–2.59", "0.036–0.038", "",
           "-0.57 to -0.73", "-3.79 to -1.21", "14.3–18.5", "62.0–90.3", "29.7–33.6"))
  
  # Generate the table using gt
  table <- data %>%
    gt() %>%
    tab_header(
      title = "Parameter estimates of the final miltefosine pharmacokinetic model (L. Verrest (2023))") %>%
    cols_label(
      Parameter = "Population parameters",
      Estimate = "Estimate",
      CI = "95% CI") %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())) %>%
    tab_source_note(
      source_note = HTML("CL<sub>TV</sub> = CL<sub>pop</sub> × (FFM<sub>i,t</sub> / FFM<sub>med</sub>)<sup>0.75</sup><br>  
    V<sub>TV</sub> = V<sub>pop</sub> × (FFM<sub>i,t</sub> / FFM<sub>med</sub>)<sup>1.00</sup><br>  
    F<sub>TV</sub> = F<sub>pop</sub> × (1 − COV<sub>F,W1</sub>) × (CD<sub>i,t</sub> / CD<sub>med</sub>)<sup>COV<sub>F,CD</sub></sup><br>
    CL, apparent oral clearance; CD, cumulative miltefosine dose (mg/kg); COV, covariate factor;
    F1, bioavailability; FFM, fat-free mass; ka, absorption rate constant;
    Q, intercompartmental clearance; Vc, central volume of distribution; 
    Vp, peripheral volume of distribution.")) %>%
    tab_footnote(
      footnote = "Obtained by SIR.",
      locations = cells_column_labels(columns = CI)) %>%
    tab_footnote(
      footnote = "Fractional change in F. Applied during the first week.",
      locations = cells_body(
        columns = Parameter,
        rows = Parameter == "COVF,W1 (fractional change)")) %>%
    tab_footnote(
      footnote = "Exponent of power relationship between cumulative dose and F. Applied after a cumulative dose of 60 mg/kg is reached.",
      locations = cells_body(
        columns = Parameter,
        rows = Parameter == "COVF,CD (exponent of power relationship)")) %>%
    fmt_markdown(columns = everything())
  
  return(table)
}
