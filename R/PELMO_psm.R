# Copyright (C) 2019  Johannes Ranke
# Contact: jranke@uni-bremen.de
# This file is part of the R package rPELMO

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>

#' Write an input file for PELMO 4.01
#'
#' This function produces, for simple cases, input files for PELMO 4.01 as distributed
#' with FOCUS PELMO 5.5.4
#'
#' @param chent Either a string with the name of the active substance, or a chent object
#' @param comment An optional comment
#' @param identifier The name of the active substance
#' @param filename The file name
#' @param path The directory where the file should be placed
#' @param overwrite If FALSE (default), the function stops if the file already exists
#' @param rate Application rates in g/ha used in each growing season reaching the soil
#' @param timing The application times. Either a numeric vector with relative
#'   application dates, or a list of vectors with absolute application dates specified as
#'   character string with the format MM-DD, named with the first three letters of the
#'   respective scenario (e.g. list(Ham = c('04-15', '04-30')) for applications on 15th
#'   and 30th of April in the Hamburg scenario.
#' @param timing_ref A vector of reference points used for relative application dates
#'   used in the timing argument. 0 is first emergence, 1 is first maturation,
#'   2 is first harvest, 3 is second emergence, 4 is second maturation, 5 is second harvest
#' @param period The period for growing seasons in which applications are made, in years.
#'   1 means annual application, 2 means biennial application, 3 means application every
#'   third year.
#' @param mw Molecular weight for parent and metabolites
#' @param diff_air Diffusion coefficient in air at 20°C in cm2/s
#' @param boundary Thickness of the boundary layer for volatilisation in cm
#' @param calc_henry Should the Henry constant of the parent compound be
#'   calculated from vapour pressure and water solubility?
#' @param henry A vector with Henry constants of the parent compound in J/mol
#'   (equivalent to SI units of Pa m3/mol) with one or two temperatures in
#'   degrees as index
#' @param cwsat A vector with one or two water solubilities of the parent
#'   compound in mg/L, named with temperatures in degrees centigrade.
#'   Temperatures should be as used for henry. If not a named vector,
#'   the water solubility measured at 20°C.
#' @param p0 A vector with one or two vapour pressures of the parent compound
#'   in Pa, named with temperatures in degrees centigrade. Temperatures should
#'   be as used for henry.  If not a named vector, the vapour pressure measured
#'   at 20°C.
#' @param Hv Presumably the enthalpy of volatilisation
#' @param PUF The plant uptake factor for the parent compound
#' @param topology Specifies the topology to be used for the transformation products.
#' @param degradation An optional list of lists with a special syntax for
#'   specifying DT50 values and formation fractions. Overrides rate constant
#'   arguments specified above (k_*), but not k_photo.
#' @param degradation_flag 0 (default) or 3: degradation according to degradation factors,
#'   1 or 4: constant with depth, 2 or 5: individual. If >2, degradation only
#'   in the liquid phase
#' @param k_photo First order photolysis rate constant to
#'   sink in 1/day
#' @param ref_photo Reference radiation for photolysis rate constant in W/m2
#' @param Q10_default Default value for the Q10 (temperature dependence of soil degradation)
#' @param moist_exp_default Default value for the soil moisture exponent
#'   (moisture dependence of soil degradation)
#' @param sorption dataframe holding Kfoc and Freundlich exponent N for OC
#'   dependent sorption for the parent and the transformation products. If no
#'   rownames are present, it is assumed that the values are in the same order
#'   as the list of transformation products in the chent object, or,
#'   if no chent object is supplied, the order of metabolites in the mw
#'   argument is used.
#' @param Freundlich_limit The lower limit of Freundlich sorption in µg/L
#' @param calc_Kd Should the Kd be calculated from the Kfoc and the organic
#'   carbon content of the soil? The alternative is only implemented for the
#'   parent compound.
#' @param horizons Data frame containing columns Kd, Freundlich exponents N and
#'   relative degradation rel_deg for direct input, overriding what has been
#'   specified in the adsorption argument for the parent compound. If
#'   degradation_flag is 2, rel_deg is used for parent compound and metabolites
#'   (which needs to be done manually in the FOCUS PELMO GUI).
#' @param deg_temp Temperature for degradation rate
#' @param Q10 The Q10 factor for temperature dependence of degradation
#' @param moist_exp The exponent for moisture dependence of degradation
#' @param moist_abs Absolute moisture
#' @param moist_rel Relative moisture
#' @param rel_deg_neq Relative degradation in non-equilibrium compartment
#' @param deg_temp_default Default temperature for degradation data
#' @param rel_deg_neq_default Default value for rel_deg_neq
#' @param moist_abs_default Default absolute moisture
#' @param moist_rel_default Default relative moisture
#' @note It is not clear how exactly the numbers in the volatilization section
#'   (henry, cwsat and p0) and their temperatures are used in the model, so
#'   it is recommended to stick to simple use cases like specifying 0 for henry
#'   constants at 20 and 30 degrees centigrade (switching off volatilisation).
#' @export
#' @author Johannes Ranke
PELMO_psm <- function(chent, comment = '',
                      identifier = if (is.character(chent)) chent
                                   else names(chent$identifier),
                      filename = paste0(identifier, ".psm"),
                      path = "E:/FOCUSPELMO.553", overwrite = FALSE,
                      rate = 100, timing = 0, timing_ref = 0, period = 1,
                      mw = chent$mw,
                      diff_air = 5e-02, boundary = 0.1, calc_henry = !is.na(p0),
                      henry = c("20" = 0, "30" = 0),
                      cwsat = NA, p0 = NA, Hv = 98400, PUF = 0,
                      topology = NA,
                      degradation = list("parent" = list(Inf)),
                      deg_temp = NA, Q10 = NA, moist_exp = NA,
                      moist_abs = NA, moist_rel = NA, rel_deg_neq = NA,
                      Q10_default = 2.58, moist_exp_default = 0.7,
                      deg_temp_default = 20, rel_deg_neq_default = 0,
                      moist_abs_default = 0, moist_rel_default = 100,
                      degradation_flag = 0,
                      k_photo = 0, ref_photo = 500,
                      sorption = data.frame(Kfoc = 0, N = 1),
                      Freundlich_limit = 0,
                      calc_Kd = TRUE,
                      horizons = data.frame(Kd = numeric(0), N = numeric(0),
                                            rel_deg = numeric(0)))
{
  file = file.path(path, filename)
  if (file.exists(file) & !overwrite) stop(file, " already exists, stopping")
  psm <- file(file, encoding = "latin1", open = "w+")
  on.exit(close(psm))

  add <- function(x) cat(paste0(x, "\r\n"), file = psm, append = TRUE)
  add0 <- function(x) cat(x, file = psm, append = TRUE)

  add("<COMMENT>")
  add(paste0(comment, "     <", identifier, ">"))
  add("<END COMMENT>")

  add("<NUMBER OF SOIL HORIZONS>")
  add(paste0(" ", nrow(horizons), " "))
  add("<END NUMBER OF SOIL HORIZONS>")

  # Calculations for application section
  n_apps = length(rate)
  n_loc = 1
  PELMO_loc_codes = c(User = "",
                      Cha = "C", Ham = "H", Jok = "J",
                      Kre = "K", Oke = "N", Pia = "P",
                      Por = "O", Sev = "S", Thi = "T")
  if (is.list(timing)) {
    absolute_apps = TRUE
    invalid_locs = setdiff(names(timing), names(PELMO_loc_codes))
    if (length(invalid_locs) != 0) stop("Invalid location(s): ", invalid_locs)
    if (is.null(timing[["User"]])) {
      PELMO_locs = c("User", names(timing))
      timing[["User"]] = "01-01"
      user_timing = FALSE
    } else {
      user_timing = TRUE
    }
    n_loc = length(timing)
  } else {
    absolute_apps = FALSE
  }
  n_apps_loc = length(rate) * (20 + 6 / period)

  add("<APPLICATION>")
  add(paste0(" ", n_loc, "    0    ", if (absolute_apps) 0 else 9,
             "    <number of locations absolute app/relative app>"))

  if (absolute_apps) {
    for (PELMO_loc in PELMO_locs) {
      add(paste0(formatC(n_apps_loc, width = 3, format = "d", flag = "0"),
                 "         ", PELMO_loc_codes[PELMO_loc], "   <number of application location>"))
      loc_rate = if (PELMO_loc != "User" | user_timing) rate else rep(0, n_apps)
      for (year in seq(from = 1, to = (6 + period * 20), by = period)) {
        for (app_nr in 1:n_apps) {
          month = substr(timing[[PELMO_loc]][app_nr], 1, 2)
          day = substr(timing[[PELMO_loc]][app_nr], 4, 5)
          add(paste0(day, "  ", month, "  ",
              formatC(year, width = 2, format = "d", flag = "0"),
              "   ",
              loc_rate[app_nr] / 1000,
              "    0    0    0    0   ",
              "<day month year app_rate app_depth frpex time>"))
        }
      }
    }
  } else {
    add(paste0(formatC(n_apps_loc, width = 3, format = "d", flag = "0"),
               "            <number of application location>"))
    for (year in seq(from = 1, to = (6 + period * 20), by = period)) {
      for (app_nr in 1:n_apps) {
        add(paste0(if (timing[app_nr] < 0) "-" else "",
                   formatC(abs(timing[app_nr]), width = 2, format = "d", flag = "0"),
            "  ",
            formatC(timing_ref, width = 2, format = "d", flag = "0"),
            "  ",
            formatC(year, width = 2, format = "d", flag = "0"),
            "   ",
            rate[app_nr] / 1000,
            "    0    0    0    0   ",
            "<day month year app_rate app_depth frpex time>"))
      }
    }
  }
  add(" 1    <pesticide appl. flag: 1=soil 2 =linear 3=exp. foliar 4=manually")
  add("<END APPLICATION>")

  # Determine henry and kd flags
  if (calc_henry) {
    henry_flag = 1
  } else {
    henry_flag = 0
  }
  if (calc_Kd) {
    kd_flag = 1
  } else {
    kd_flag = 0
  }

  add("<FLAGS>")
  add(paste0(" ", henry_flag, "   ", kd_flag, "   <henry(0=direct 1=calc.)  kd_flag(0=direct 1=calc.)>"))
  add("<END FLAGS>")

  # Set defaults for 30°C as in the PELMO GUI, if only single values are given
  if (is.null(names(p0))) p0 <- c("20" = p0, "30" = signif(4 * p0, 3))
  if (is.null(names(cwsat))) cwsat <- c("20" = cwsat, "30" = signif(2 * cwsat, 3))
  add("<VOLATILIZATION>")
  add("<henry       solub.      molmass     vap.press   diff air    depth volat.  Hv        Temperature>")

  temperatures = names(henry)
  for (i in 1:2) {
    temperature = temperatures[i]
    p0_temp <- if (is.na(p0[temperature])) 0 else p0[temperature]
    cwsat_temp <- if (is.na(cwsat[temperature])) 0 else cwsat[temperature]
    add(paste0("  ", formatC(henry[temperature], format = "E", digits = 2),
               "   ", cwsat_temp,
               "    ", mw[1],
               "     ", formatC(p0_temp, format = "E", digits = 2), "   ",
               diff_air, "    ",
               boundary, "    ",
               Hv, "    ",
               temperature, " "))
  }
  add("<END VOLATILIZATION>")

  add("<PLANT UPTAKE>")
  add(paste0(" ", PUF, "      <plant uptake factor"))
  add("<END PLANT UPTAKE>")

  # Calculations for topology and degradation rates
  mets = paste0(rep(LETTERS[1:4], 2), rep(1:2, each = 4))
  k = matrix(NA, nrow = 9, ncol = 9, dimnames = list(from = c("parent", mets), to = c(mets, "sink")))
  # Set the pathways accessible via the PELMO GUI to 0
  k["parent", c(1:4, 9)] = 0
  k["A1", c("A2", "B2", "B1", "sink")] = 0
  k["B1", c("B2", "C2", "C1", "sink")] = 0
  k["C1", c("C2", "D2", "D1", "sink")] = 0
  k["D1", c("D2", "sink")] = 0
  k["A2", c("B2", "sink")] = 0
  k["B2", c("C2", "sink")] = 0
  k["C2", c("D2", "sink")] = 0
  k["D2", "sink"] = 0

  topology["parent"] = "parent"
  if (is.list(degradation)) {
    for (compound in names(degradation)) {
      DT50 = degradation[[compound]][[1]]
      k_compound = log(2)/DT50
      ff_sink = 1
      comp = topology[compound]
      if (length(degradation[[compound]]) == 2) {
        ff = degradation[[compound]][[2]]
        if (sum(ff) > 1) stop("Formation fractions for ", compound, " exceed one, not supported by PELMO")
        targets = names(ff)

        for (target in targets) {

          met = topology[target]
          if (is.na(met)) {
            possible_paths = sort(names(which(k[comp, -9] == 0)))
            if (length(possible_paths) == 0) {
              stop("Could not establish topology for ", target)
            } else {
              met = possible_paths[1]
              topology[target] = met
            }
          } else {
            if (is.na(k[comp, met])) {
              stop("Path from ", compound, " (", comp, ") to ",
                   target, " (", met, ") not possible.")
            }
          }
          k[comp, met] = k_compound * ff[target]
          ff_sink = ff_sink - ff[target]
        }
      }
      k[comp, "sink"] = k_compound * ff_sink
    }
  }

  # Function to set degradation parameters for each pathway
  check_degpar <- function(degpar, default, from = "parent", to) {
    if (is.list(degpar)) {
      if (!is.na(degpar[[from]][to])) {
        degpar[[from]][to]
      } else default
    } else default
  }

  add("<DEGRADATION>")
  add("<degrate    degtemp     q10         moist-abs   moist-rel   moist-exp   rel deg neq sites")

  for (path in c("A1", "B1", "C1", "D1", "sink")) {
    k_num <- k["parent", path]
    k_string <- if (k_num > 0) formatC(round(k_num, 6), digits = 7, format = 'f') else " 0.00E+00"
    temp_num <- check_degpar(deg_temp, deg_temp_default, to = path)
    Q10_num <- check_degpar(Q10, Q10_default, to = path)
    moist_abs_num <- check_degpar(moist_abs, moist_abs_default, to = path)
    moist_rel_num <- check_degpar(moist_rel, moist_rel_default, to = path)
    moist_exp_num <- check_degpar(moist_exp, moist_exp_default, to = path)
    rel_deg_neq_num <- check_degpar(rel_deg_neq, rel_deg_neq_default, to = path)
    add(paste0(" ", k_string, "   ",
               formatC(temp_num, width = 8, flag = "-"), "    ",
               formatC(Q10_num, width = 8, flag = "-"), "    ",
               formatC(moist_abs_num, width = 8, flag = "-"), "    ",
               formatC(moist_rel_num, width = 8, flag = "-"), "    ",
               formatC(moist_exp_num, width = 8, flag = "-"), "  ",
               formatC(rel_deg_neq_num, width = 8, flag = "-"), " <",
               if (path == "sink") "BR/CO2" else paste("Met", path),
               ">"))
  }
  add(paste0(" ", degradation_flag,
             "                 <0 = degradation according to degradation factors,",
             " 1 = constant with depth,2 individual,",
             "3 = degradation according to degradation factors,",
             " 4 = constant with depth,5 individual, >2 deg in liquid phase only)"))

  add("<PHOTODEGRADATION>")
  add("<rate 1/n>  <I_Ref W/m\u00B2>")

  for (i in 1:5) {
    k_photo_path = if (length(k_photo) > 1) k_photo[i] else k_photo
    ref_photo_path = if (length(ref_photo) > 1) ref_photo[i] else ref_photo

    add0(" ")
    add0(k_photo_path)
    add0("   ")
    add0(ref_photo_path)
    add0(" ")
  }

  add("")
  add("<END DEGRADATION>")

  add("<ADSORPTION>")
  add(paste0("<Koc-value    Fr.exp.Koc  pH          pKa     limit for Freundl.  ann.incr.>",
             " <k_doc> <% change> KOC2      pH2 f_neq       kdes>"))
  add(paste0(" ", formatC(sorption[["Kfoc"]][1], width = 10, flag = "-"),
             "  ", formatC(sorption[["N"]][1], width = 6, flag = "-"),
             "     -99          20          ",
             formatC(Freundlich_limit[1], width = 4, flag = "-"),
             "             0      0     1    -99    -99     0     0 "))
  add("<END ADSORPTION>")

  add("<DEPTH DEPENDENT SORPTION AND TRANSFORMATION VALUES>")
  add("<Kd          Fr.exp.     Met A1   Met B1   Met C1   Met D1   BR/CO2 >")

  if (nrow(horizons) > 0) {
    for (i in 1:nrow(horizons)) {
      add(paste0(" ", formatC(horizons[i, "Kd"], width = 10, flag = "-"),
                 "  ", formatC(horizons[i, "N"], width = 10, flag = "-"),
                 "  ", formatC(horizons[i, "rel_deg"], width = 6, flag = "-"),
                 "   ", formatC(horizons[i, "rel_deg"], width = 6, flag = "-"),
                 "   ", formatC(horizons[i, "rel_deg"], width = 6, flag = "-"),
                 "   ", formatC(horizons[i, "rel_deg"], width = 6, flag = "-"),
                 "   ", formatC(horizons[i, "rel_deg"], width = 6, flag = "-"),
                 " <horizon ", i, " >"))
    }
  }

  add("<END DEPTH DEPENDENT>")

  # Metabolites
  mets = intersect(mets, topology)

  if (nrow(horizons) > 0) { # Set the horizon specific sorption data to GUI defaults
    horizons$Kd <- 0
    horizons$N <- 0.9
  }

  if (length(mets) > 0) {
    inv_topology = names(topology)
    names(inv_topology) = topology
    if (all(rownames(sorption) == as.character(1:nrow(sorption)))) {
      rownames(sorption) = if (inherits(chent, "chent")) {
        c("parent", sapply(chent$TPs, function(x) x$identifier))
      } else {
        names(mw)
      }
    }

    for (met in intersect(mets, topology)) {
      met_name = inv_topology[met]
      add("###############################################")
      add("<COMMENT>")
      add(paste0("METABOLITE ", met, "     ", met_name))
      add("<END COMMENT>")

      add("<FLAGS>")
      add(paste0(" 1   <kd_flag(0=direct 1=calc.)>"))
      add("<END FLAGS>")

      add("<MOLMASS>")
      mw_met = if (inherits(chent, "chent")) {
        round(chent$TPs[[make.names(met_name)]]$mw, 1)
      } else {
        mw[met_name]
      }
      add(paste0(" ", mw_met, " "))
      add("<END MOLMASS>")
      add("<PLANT UPTAKE>")
      add(" 0      <plant uptake factor>")
      add("<END PLANT UPTAKE>")
      add("<DEGRADATION>")
      add(paste0("<degrate    degtemp     q10         moist-abs   moist-rel   moist-exp  ",
                 "<rel deg in neq sites>"))
      paths = names(which(!is.na(k[met, ])))
      for (path in paths) {
        k_num <- k[met, path]
        k_string <- if (k_num > 0) formatC(round(k_num, 6), digits = 7, format = 'f') else " 0.00E+00"
        temp_num <- check_degpar(deg_temp, deg_temp_default, from = met, to = path)
        Q10_num <- check_degpar(Q10, Q10_default, from = met, to = path)
        moist_abs_num <- check_degpar(moist_abs, moist_abs_default, from = met, to = path)
        moist_rel_num <- check_degpar(moist_rel, moist_rel_default, from = met, to = path)
        moist_exp_num <- check_degpar(moist_exp, moist_exp_default, from = met, to = path)
        rel_deg_neq_num <- check_degpar(rel_deg_neq, rel_deg_neq_default, from = met, to = path)
        add(paste0(" ", k_string, "   ",
                   formatC(temp_num, width = 8, flag = "-"), "    ",
                   formatC(Q10_num, width = 8, flag = "-"), "    ",
                   formatC(moist_abs_num, width = 8, flag = "-"), "    ",
                   formatC(moist_rel_num, width = 8, flag = "-"), "    ",
                   formatC(moist_exp_num, width = 8, flag = "-"), "  ",
                   formatC(rel_deg_neq_num, width = 8, flag = "-"), " <",
                   if (path == "sink") "BR/CO2" else paste("Met", path), ">"))
      }
      add(paste0(" ", degradation_flag, "                 <0 = degradation according to degradation factors,",
                 " 1 = constant with depth,2 individual,",
                 "3 = degradation according to degradation factors,",
                 " 4 = constant with depth,5 individual, >2 deg in liquid phase only)"))
      add("<END DEGRADATION>")

      add("<ADSORPTION>")
      add(paste0("<Koc-value    Fr.exp.Koc  pH          pKa     limit for Freundl.  ann.incr.>",
                 " <k_doc> <% change> KOC2      pH2    f_neq       kdes>"))
      add(paste0(" ", formatC(sorption[met_name, "Kfoc"], digits = 6, width = 10, flag = "-"),
                 "  ", formatC(sorption[met_name, "N"], width = 6, flag = "-"),
                 "     -99          20          ",
                 formatC(Freundlich_limit, width = 4, flag = "-"),
                 "             0      0     1    -99    -99     0     0 "))
      add("<END ADSORPTION>")

      add("<DEPTH DEPENDENT SORPTION AND TRANSFORMATION VALUES>")
      add(paste0("<Kd          Fr.exp.     ",
                 paste0("Met ", paths[-length(paths)], collapse = "   "), "   BR/CO2 >"))
      if (nrow(horizons) > 0) {
        for (i in 1:nrow(horizons)) {
          add0(paste0(" ", formatC(horizons[i, "Kd"], width = 10, flag = "-"),
                     "  ", formatC(horizons[i, "N"], width = 10, flag = "-")))
          for (j in 1:length(paths)) {
            add0(paste0("  ", formatC(horizons[i, "rel_deg"], width = 7, flag = "-")))
          }
          add(paste0(" <horizon ", i, " >"))
        }
      }
      add("<END DEPTH DEPENDENT>")
    }
  }

  add("<END PSM>")
}
