# Copyright (C) 2017-2019  Johannes Ranke
# Contact: jranke@uni-bremen.de
# This file is part of the R package rPELMO
# Until January 2019 it was part of the R package pfm

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

#' FOCUS PELMO crop names
#'
#' A named character vector with the crop names used in the PELMO 5.5.3 GUI.
#' For the names, three letter codes were constructed by generally taking the
#' first three letters in lower case. Only when there is an expression in
#' parentheses, the first letter of this expression becomes the first letter
#' in the three letter code, i.e. 'Peas (animals)' has the code \code{ape}.
#'
#' @docType data
#' @export
#' @examples
#' print(FOCUS_PELMO_crops)
FOCUS_PELMO_crops = c(
  app = "Apples",
  gra = "Grass and alfalfa",
  pot = "Potatoes",
  sug = "Sugar beets",
  win = "Winter cereals",
  fbe = "Beans (field)",
  vbe = "Beans (vegetables)",
  bus = "Bushberries",
  cab = "Cabbage",
  car = "Carrots",
  cit = "Citrus",
  cot = "Cotton",
  lin = "Linseed",
  mai = "Maize",
  soi = "Oil seed rape (summer)",
  woi = "Oil seed rape (winter)",
  oni = "Onions",
  ape = "Peas (animals)",
  soy = "Soybeans",
  spr = "Spring cereals",
  str = "Strawberries",
  sun = "Sunflower",
  tob = "Tobacco",
  tom = "Tomatoes",
  vin = "Vines")

#' FOCUS PELMO crop acronyms used for naming sceneario files
#'
#' A named character vector with the crop acronyms used in FOCUS PELMO 5.5.3
#' for naming the .sze files located in the FOCUS directory. The crop acronyms
#' in the file names are sometimes capitalized, sometimes not. The scenario
#' files used for Beans (field) and Beans (vegetable) are the same.
#'
#' @docType data
#' @export
#' @examples
#' print(FOCUS_PELMO_crop_sze_names)
FOCUS_PELMO_crop_sze_names = c(
  app = "apples",
  gra = "grass",
  pot = "potato",
  sug = "sbeets",
  win = "wcerea",
  fbe = "beans",
  vbe = "beans", # Same sze as for fbe is used, with 'irrigation' which has no effect on the run
  bus = "bushb",
  cab = "cabbag",
  car = "carrot",
  cit = "citrus",
  cot = "cotton",
  lin = "linse",
  mai = "maize",
  soi = "rapesu",
  woi = "rapewi",
  oni = "onions",
  ape = "peas",
  soy = "soyb",
  spr = "scerea",
  str = "strawb",
  sun = "sunflo",
  tob = "tobacc",
  tom = "tomato",
  vin = "vines")

#' Location codes in FOCUS PELMO
#'
#' A named character vector of one letter location codes used in FOCUS PELMO,
#' indexed by three letter acronyms.

#' @docType data
#' @export
#' @examples
#' print(FOCUS_PELMO_location_codes)
FOCUS_PELMO_location_codes = c(
  Cha = "C", Ham = "H", Jok = "J",
  Kre = "K", Oke = "N", Pia = "P",
  Por = "O", Sev = "S", Thi = "T")
