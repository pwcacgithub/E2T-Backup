interface ZIF_HR_CONSTANTS_UTILITY
  public .
constants C_PLANVERSION01 type PLVAR value '01'. "#EC NOTEXT
  constants C_STATUS1 type ISTAT_D value '1'. "#EC NOTEXT
  constants C_OTYPEPOSITION type OTYPE value 'S'. "#EC NOTEXT
  constants C_OTYPEORGUNIT type OTYPE value 'O'. "#EC NOTEXT
  constants C_OTYPEJOB type OTYPE value 'C'. "#EC NOTEXT
  constants C_OTYPEPERSON type OTYPE value 'P'. "#EC NOTEXT
  constants C_OPERMODENEWENDDA type HRBAS_TIMEPERIOD_CHANGE_MODE value '2'. "#EC NOTEXT
  constants C_VACSTATOPEN type VACAN_STAT value '0'. "#EC NOTEXT
  constants C_VACSTATFILLED type VACAN_STAT value '2'. "#EC NOTEXT
  constants C_OPENVACANCY type VACAN value 'X'. "#EC NOTEXT
  constants C_CLOSEDVACANCY type VACAN value SPACE. "#EC NOTEXT
  constants C_EXECMODEOBJECTPART type HRBAS_EXECUTE_MODE value '1'. "#EC NOTEXT
  constants C_EXECMODEINFOTYPE type HRBAS_EXECUTE_MODE value '0'. "#EC NOTEXT
  constants C_VACANDECISIONOPEN type char7 value 'OPEN'. "#EC NOTEXT
  constants C_VACANDECISIONDELIM type char7 value 'DELI'. "#EC NOTEXT
  constants C_OTYPEUSER type OTYPE value 'US'. "#EC NOTEXT
  constants C_OMTOPDOWN type RSIGN value 'B'. "#EC NOTEXT
  constants C_OMBOTTOMUP type RSIGN value 'A'. "#EC NOTEXT
  constants C_OTYPEKOSTL type OTYPE value 'K'. "#EC NOTEXT

endinterface.
