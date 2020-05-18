pocty_clenu_skautis_cols <- cols(
  Year = col_integer(),
  ID_Unit = col_integer(),
  ID_UnitType = col_character(),
  RegistrationNumber = col_character(),
  UnitName = col_character(),
  Location = col_character(),
  RegularMembersTo6 = col_integer(),
  RegularMembersTo15 = col_integer(),
  RegularMembersTo18 = col_integer(),
  RegularMembersTo26 = col_integer(),
  RegularMembersFrom26 = col_integer(),
  RegularMembers = col_integer(),
  MembersTo6 = col_integer(),
  MembersTo15 = col_integer(),
  MembersTo18 = col_integer(),
  MembersTo26 = col_integer(),
  MembersFrom26 = col_integer(),
  Members = col_integer()
)


nacti_skautis_pocty_clenu <- function(file) {
  read_delim(file, delim = ";", na = c("NULL"),
          col_types = pocty_clenu_skautis_cols)
}
