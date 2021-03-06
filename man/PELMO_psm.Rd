% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PELMO_psm.R
\name{PELMO_psm}
\alias{PELMO_psm}
\title{Write an input file for PELMO 4.01}
\usage{
PELMO_psm(chent, comment = "", identifier = if (is.character(chent))
  chent else names(chent$identifier), filename = paste0(identifier,
  ".psm"), path = "E:/FOCUSPELMO.553", overwrite = FALSE, rate = 100,
  timing = 0, timing_ref = 0, period = 1, mw = chent$mw,
  diff_air = 0.05, boundary = 0.1, calc_henry = !is.na(p0),
  henry = c(`20` = 0, `30` = 0), cwsat = NA, p0 = NA, Hv = 98400,
  PUF = 0, topology = NA, degradation = list(parent = list(Inf)),
  deg_temp = NA, Q10 = NA, moist_exp = NA, moist_abs = NA,
  moist_rel = NA, rel_deg_neq = NA, Q10_default = 2.58,
  moist_exp_default = 0.7, deg_temp_default = 20,
  rel_deg_neq_default = 0, moist_abs_default = 0,
  moist_rel_default = 100, degradation_flag = 0, k_photo = 0,
  ref_photo = 500, sorption = data.frame(Kfoc = 0, N = 1),
  Freundlich_limit = 0, calc_Kd = TRUE, horizons = data.frame(Kd =
  numeric(0), N = numeric(0), rel_deg = numeric(0)))
}
\arguments{
\item{chent}{Either a string with the name of the active substance, or a chent object}

\item{comment}{An optional comment}

\item{identifier}{The name of the active substance}

\item{filename}{The file name}

\item{path}{The directory where the file should be placed}

\item{overwrite}{If FALSE (default), the function stops if the file already exists}

\item{rate}{Application rates in g/ha used in each growing season reaching the soil}

\item{timing}{The application times. Either a numeric vector with relative
application dates, or a list of vectors with absolute application dates specified as
character string with the format MM-DD, named with the first three letters of the
respective scenario (e.g. list(Ham = c('04-15', '04-30')) for applications on 15th
and 30th of April in the Hamburg scenario.}

\item{timing_ref}{A vector of reference points used for relative application dates
used in the timing argument. 0 is first emergence, 1 is first maturation,
2 is first harvest, 3 is second emergence, 4 is second maturation, 5 is second harvest}

\item{period}{The period for growing seasons in which applications are made, in years.
1 means annual application, 2 means biennial application, 3 means application every
third year.}

\item{mw}{Molecular weight for parent and metabolites}

\item{diff_air}{Diffusion coefficient in air at 20°C in cm2/s}

\item{boundary}{Thickness of the boundary layer for volatilisation in cm}

\item{calc_henry}{Should the Henry constant of the parent compound be
calculated from vapour pressure and water solubility?}

\item{henry}{A vector with Henry constants of the parent compound in J/mol
(equivalent to SI units of Pa m3/mol) with one or two temperatures in
degrees as index}

\item{cwsat}{A vector with one or two water solubilities of the parent
compound in mg/L, named with temperatures in degrees centigrade.
Temperatures should be as used for henry. If not a named vector,
the water solubility measured at 20°C.}

\item{p0}{A vector with one or two vapour pressures of the parent compound
in Pa, named with temperatures in degrees centigrade. Temperatures should
be as used for henry.  If not a named vector, the vapour pressure measured
at 20°C.}

\item{Hv}{Presumably the enthalpy of volatilisation}

\item{PUF}{The plant uptake factor for the parent compound}

\item{topology}{Specifies the topology to be used for the transformation products.}

\item{degradation}{An optional list of lists with a special syntax for
specifying DT50 values and formation fractions. Overrides rate constant
arguments specified above (k_*), but not k_photo.}

\item{deg_temp}{Temperature for degradation rate}

\item{Q10}{The Q10 factor for temperature dependence of degradation}

\item{moist_exp}{The exponent for moisture dependence of degradation}

\item{moist_abs}{Absolute moisture}

\item{moist_rel}{Relative moisture}

\item{rel_deg_neq}{Relative degradation in non-equilibrium compartment}

\item{Q10_default}{Default value for the Q10 (temperature dependence of soil degradation)}

\item{moist_exp_default}{Default value for the soil moisture exponent
(moisture dependence of soil degradation)}

\item{deg_temp_default}{Default temperature for degradation data}

\item{rel_deg_neq_default}{Default value for rel_deg_neq}

\item{moist_abs_default}{Default absolute moisture}

\item{moist_rel_default}{Default relative moisture}

\item{degradation_flag}{0 (default) or 3: degradation according to degradation factors,
1 or 4: constant with depth, 2 or 5: individual. If >2, degradation only
in the liquid phase}

\item{k_photo}{First order photolysis rate constant to
sink in 1/day}

\item{ref_photo}{Reference radiation for photolysis rate constant in W/m2}

\item{sorption}{dataframe holding Kfoc and Freundlich exponent N for OC
dependent sorption for the parent and the transformation products. If no
rownames are present, it is assumed that the values are in the same order
as the list of transformation products in the chent object, or,
if no chent object is supplied, the order of metabolites in the mw
argument is used.}

\item{Freundlich_limit}{The lower limit of Freundlich sorption in µg/L}

\item{calc_Kd}{Should the Kd be calculated from the Kfoc and the organic
carbon content of the soil? The alternative is only implemented for the
parent compound.}

\item{horizons}{Data frame containing columns Kd, Freundlich exponents N and
relative degradation rel_deg for direct input, overriding what has been
specified in the adsorption argument for the parent compound. If
degradation_flag is 2, rel_deg is used for parent compound and metabolites
(which needs to be done manually in the FOCUS PELMO GUI).}
}
\description{
This function produces, for simple cases, input files for PELMO 4.01 as distributed
with FOCUS PELMO 5.5.4
}
\note{
It is not clear how exactly the numbers in the volatilization section
  (henry, cwsat and p0) and their temperatures are used in the model, so
  it is recommended to stick to simple use cases like specifying 0 for henry
  constants at 20 and 30 degrees centigrade (switching off volatilisation).
}
\author{
Johannes Ranke
}
