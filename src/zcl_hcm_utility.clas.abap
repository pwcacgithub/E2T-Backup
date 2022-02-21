class ZCL_HCM_UTILITY definition
  public
  final
  create public .

public section.

  interfaces ZIF_HR_CONSTANTS_UTILITY .

  class-methods READ_INFOTYPE
    importing
      !BEGINDATE type BEGDA
      !ENDDATE type ENDDA
      !MODE type HRPAD_READ_MODE default '1'
      !NOAUTHCHECK type BOOLE_D default ABAP_TRUE
      !PERSONNELNUMBER type PERNR_D
      !TCLAS type TCLAS default 'A'
      !INFOTYPE type INFTY
      !SUBTYPE type SUBTY optional
      !OBJPS type OBJPS optional
      !SPRPS type SPRPS optional
    exporting
      !PNNNN type ANY
      !DATAEXISTS type BOOLE_D .
  class-methods GET_PAYROLL_YEAR_PERIOD
    importing
      !PAYROLLAREA type ABKRS
      !KEYDATE type DATUM
    exporting
      !PAYROLLYEAR type PABRJ
      !PAYROLLPERIOD type PABRP .
  class-methods GET_DAYS_MONTHS_IN_ROLE
    importing
      !KEYDATE type DATUM
      !PERSONNELNUMBER type PERNR_D
      !PAYROLLAREA type ABKRS
    exporting
      !DAYSINROLE type DBNUM
      !MONTHSINROLE type DBNUM .
  class-methods GET_OBJECT_SHORT_TEXT
    importing
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !PLANVERSION type PLVAR default '01'
      !KEYDATE type DATUM
      !STATUS type ISTAT_D default '1'
    returning
      value(SHORTTEXT) type SHORT_D .
  class-methods GET_PAYROLL_PERIOD_BEGIN_DATES
    importing
      !PAYROLLAREA type ABKRS
      !KEYDATE type DATUM
    exporting
      !CURRENTPERIODBEGIN type BEGDA
      !NEXTPERIODBEGIN type BEGDA
      !ISOK type BOOLE_D .
  class-methods GET_MANAGER_FOR_EMPLOYEE
    importing
      !PERSONNELNUMBER type PERNR_D
      !KEYDATE type BEGDA
    exporting
      value(MANAGERNUMBER) type PERNR_D
      !MANAGERNAME type EMNAM .
  class-methods GET_CHIEF_OF_ORG_UNIT
    importing
      !ORGANIZATIONUNIT type HROBJID
      !EVALPATH type WEGID default 'O-O'
      !PLANVERSION type PLVAR default '01'
      !KEYDATE type DATS default SY-DATUM
      !AUTHCHECK type HR_AUTHY default SPACE
      !OBJECTTYPE type OTYPE default 'O'
      !EXCLUDEFIRSTLEVEL type BOOLE_D default SPACE
    exporting
      !MANAGERNUMBER type PERNR_D
      !MANAGERNAME type STEXT .
  class-methods IS_EMPLOYEE_A_MANAGER
    importing
      !PERSONNELNUMBER type PERNR_D
      !EVALPATH type WEGID default 'MANASS'
      !PLANVERSION type PLVAR default '01'
      !KEYDATE type DATS default SY-DATUM
      !AUTHCHECK type HR_AUTHY default SPACE
      !OBJECTTYPE type OTYPE default 'P'
    returning
      value(ISMANAGER) type BOOLE_D .
  class-methods READ_RELATIONSHIP_INFOPTYPES
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type OBJEKTID
      !STATUS type ISTAT_D default '1'
      !INFTY type INFOTYP default '1001'
      !SUBTY type SUBTYP
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default SY-DATUM
      !NOAUTHCHECK type BOOLE_D default 'X'
    exporting
      !RELATIONSHIPS type P1001TAB
    changing
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER .
  class-methods GET_CHIEF_OF_ORG_UNIT_AT_LEVEL
    importing
      !ORGANIZATIONUNIT type HROBJID
      !EVALPATH type WEGID default 'O-O'
      !PLANVERSION type PLVAR default '01'
      !KEYDATE type DATUM default SY-DATUM
      !AUTHCHECK type BOOLE_D default SPACE
      !OBJECTTYPE type OTYPE default 'O'
      !LEVEL type INT4
      !RETURNOTYPE type OTYPE default 'P'
    exporting
      !RECIPIENTS type SWFUAGENTS .
  class-methods GET_EMAIL_ADDRESS
    importing
      !PERSONNELNUMBER type PERNR_D
      !BEGINDATE type BEGDA
    returning
      value(EMAILADDRESS) type STRING
    raising
      ZCX_NO_DATA_EXISTS .
  class-methods GET_EMPLOYEE_KNOW_AS_NAME
    importing
      !PERSONNELNUMBER type PERNR_D
      !BEGINDATE type BEGDA
    returning
      value(EMPLOYEENAME) type EMNAM
    raising
      ZCX_NO_DATA_EXISTS .
  class-methods GET_EMPLOYEE_NAME
    importing
      !PERSONNELNUMBER type PERNR_D
      !BEGINDATE type BEGDA
    returning
      value(EMPLOYEENAME) type EMNAM
    raising
      ZCX_NO_DATA_EXISTS .
  class-methods GET_ORGUNIT_OF_POSITION
    importing
      !POSITIONID type HROBJID
      !KEYDATE type DATUM default SY-DATUM
      !AUTHCHECK type BOOLE_D default SPACE
    returning
      value(ORGUNITID) type HROBJID .
  class-methods GET_PERSONNEL_NUMBER
    importing
      !USERNAME type SYUNAME default SY-UNAME
      !BEGINDATE type DATUM default SY-DATUM
    returning
      value(PERSONNELNUMBER) type PERNR_D
    raising
      ZCX_NO_DATA_EXISTS .
  class-methods IS_EMPLOYEE
    importing
      !I_PERNR type PERSNO optional
      !I_USRID type SYSID optional
    returning
      value(R_IS_EMPLOYEE) type BOOLE_D .
  class-methods IS_POSITION_OCCUPIED
    importing
      !POSITIONID type HROBJID
      !AUTHCHECK type BOOLE_D default SPACE
      !KEYDATE type DATUM default SY-DATUM
    exporting
      !HASHOLDER type BOOLE_D .
  class-methods READ_INFOTYPE_TC3
    importing
      !BEGINDATE type BEGDA default SY-DATUM
      !ENDDATE type ENDDA default SY-DATUM
      !MODE type HRPAD_READ_MODE default '1'
      !NOAUTHCHECK type BOOLE_D default 'X'
      !PERSONNELNUMBER type PERNR_D
      !TCLAS type TCLAS default 'A'
      !INFOTYPE type INFTY
      !SUBTYPE type SUBTY
      !INFOTYPEVERSION type ITVERS default '07'
      !OBJPS type OBJPS optional
      !SPRPS type SPRPS optional
    exporting
      !INFOTYPETAB type HRPAD_INFTY_CONTAINER_TAB
      !DATAEXISTS type BOOLE_D .
  class-methods IS_POSITION_A_MANAGER
    importing
      !POSITIONID type HROBJID
      !EVALPATH type WEGID default 'A012'
      !PLANVERSION type PLVAR default '01'
      !KEYDATE type DATS default SY-DATUM
      !AUTHCHECK type HR_AUTHY default SPACE
      !OBJECTTYPE type OTYPE default 'S'
    returning
      value(ISMANAGER) type BOOLE_D .
  class-methods GET_POSITION_OF_EMPLOYEE
    importing
      !PERSONNELNUMBER type HROBJID
      !EVALPATH type WEGID default 'B008'
      !PLANVERSION type PLVAR default '01'
      !KEYDATE type DATS default SY-DATUM
      !AUTHCHECK type HR_AUTHY default SPACE
      !OBJECTTYPE type OTYPE default 'P'
    returning
      value(POSITIONID) type HROBJID
    raising
      ZCX_NO_DATA_EXISTS .
  class-methods CREATE_RELATIONSHIP
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !INFOTYPE type INFTY default '1001'
      !SUBTYPE type SUBTY
      !ISTAT type ISTAT_D default '1'
      !BEGINDATE type BEGDA default SY-DATUM
      !ENDDATE type ENDDA default '99991231'
      !SCLAS type SCLAS
      !SOBID type SOBID
      !FLUSH type WDY_BOOLEAN default ABAP_FALSE
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
    exporting
      !ISOK type WDY_BOOLEAN
      !NEWCONTAINER type ref to IF_HRBAS_INFTY_CONTAINER .
  class-methods DELETE_RELATIONSHIP
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !INFOTYPE type INFTY default '1001'
      !SUBTYPE type SUBTY
      !STATUS type ISTAT_D default '1'
      !BEGINDATE type BEGDA default SY-DATUM
      !ENDDATE type ENDDA default SY-DATUM
      !SCLAS type SCLAS
      !SOBID type SOBID
      !FLUSH type WDY_BOOLEAN
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
    exporting
      !ISOK type WDY_BOOLEAN .
  class-methods GET_POSITION_HOLDERS
    importing
      !POSITIONID type HROBJID
      !AUTHCHECK type BOOLE_D default SPACE
      !KEYDATE type DATUM default SY-DATUM
    returning
      value(HOLDERS) type TSWHACTOR
    raising
      ZCX_NO_DATA_EXISTS .
  class-methods GET_USERNAME_FOR_EMPLOYEE
    importing
      !PERSONNELNUMBER type PERNR_D
      !DATE type DATUM default SY-DATUM
    returning
      value(USERNAME) type XUBNAME
    raising
      ZCX_NO_DATA_EXISTS .
  class-methods READ_PD_PLANNED_COMP_SINGLE
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !INFTY type INFTY default '1005'
      !SUBTY type SUBTY default SPACE
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default SY-DATUM
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
      !STATUS type ISTAT_D default '1'
    exporting
      !ISOK type WDY_BOOLEAN
      !P1005 type P1005 .
  class-methods READ_PD_ENT_STRUCTURE
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !INFTY type INFTY default '1008'
      !SUBTY type SUBTY default SPACE
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default SY-DATUM
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
      !STATUS type ISTAT_D default '1'
    exporting
      !ISOK type WDY_BOOLEAN
      !P1008 type P1008 .
  class-methods READ_PD_OBJECT
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !INFTY type INFTY default '1000'
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default SY-DATUM
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
      !STATUS type ISTAT_D default '1'
    exporting
      !ISOK type WDY_BOOLEAN
      !P1000 type P1000 .
  class-methods GET_ALL_VACANCIES_BELOW_ORG
    importing
      !ORGANIZATIONUNIT type HROBJID
      !KEYDATE type DATUM default SY-DATUM
      !AUTHCHECK type WDY_BOOLEAN default ABAP_FALSE
      !PLANVERSION type PLVAR default '01'
    returning
      value(VACANCIES) type ZTT_POSITIONS .
  class-methods READ_PD_VACANCY
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !INFTY type INFTY default '1007'
      !SUBTY type SUBTY default SPACE
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default SY-DATUM
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
      !STATUS type ISTAT_D default '1'
    exporting
      !ISOK type WDY_BOOLEAN
      !P1007 type P1007 .
  class-methods READ_PD_INFOTYPE_SINGLE
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE default 'S'
      !OBJECTID type HROBJID
      !INFTY type INFTY optional
      !SUBTY type SUBTY default SPACE
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default SY-DATUM
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
      !STATUS type ISTAT_D default '1'
    exporting
      !ISOK type WDY_BOOLEAN
      !PNNNN type ANY .
  class-methods GET_ACCRED_SERVICE_DATE
    importing
      !KEYDATE type DATUM
      !PERSONNELNUMBER type PERNR_D
      !PAYROLLAREA type ABKRS
    exporting
      !ASYEARS type DBNUM
      !ASMONTHS type DBNUM .
  class-methods READ_EEGROUP_SUBGROUP
    importing
      !PLANVERSION type PLVAR default '01'
      !OBJECTTYPE type OTYPE
      !OBJECTID type HROBJID
      !INFTY type INFTY default '1013'
      !SUBTY type SUBTY default SPACE
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default SY-DATUM
      !MESSAGEHANDLER type ref to IF_HRBAS_MESSAGE_HANDLER
      !NOAUTHCHECK type WDY_BOOLEAN default ABAP_TRUE
      !STATUS type ISTAT_D default '1'
    exporting
      !ISOK type WDY_BOOLEAN
      !P1013 type P1013 .
protected section.
*"* protected components of class ZCL_HCMPA_MSS_HELPER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_HCM_MSS_HELPER
*"* do not include other source files here!!!

  constants CDAYSINROLE type PT_ZTART value 'Z101'. "#EC NOTEXT
  constants CMONTHSINROLE type PT_ZTART value 'Z102'. "#EC NOTEXT
  constants CASMONTHS type PT_ZTART value 'Z005'. "#EC NOTEXT
  constants CASYEARS type PT_ZTART value 'Z004'. "#EC NOTEXT
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
ENDCLASS.



CLASS ZCL_HCM_UTILITY IMPLEMENTATION.


method create_relationship.

* create relat
******** i haven't included adata here yet but as its in helper it should be there eventually

  data pdbuslogic type ref to if_hrbas_infty_bl.
  data updatemode type hrbas_update_mode.
  data key type hripkey.
  data container type ref to if_hrbas_infty_container.
  data p1001 type ref to data.
  data newcontdata type ref to if_hrbas_infty_container_data.
  data msg type symsg.
  data rhombuffer type ref to if_hrbas_infty_buffer.
  data clrhombuffer type ref to cl_hrbas_rhombuffer.
  data varyf type varyf.

  field-symbols <p1001> type p1001.

  cl_hrbas_rhombuffer=>get_instance( importing infty_buffer = rhombuffer ).
  clrhombuffer ?= rhombuffer.
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infotype ).

* update the key
  key-mandt = sy-mandt.
  key-plvar = planversion.
  key-otype = objecttype.
  key-objid = objectid.
  key-infty = infotype.
  key-subty = subtype.
  key-istat = istat.
  key-begda = begindate.
  key-endda = enddate.
  if numofchar( sclas ) = 1.
    concatenate sclas sobid into varyf separated by space.
  else.
    concatenate sclas sobid into varyf.
  endif.
  key-varyf = varyf.

  pdbuslogic->get_infty_container(
              exporting
                hripkey = key
                no_auth_check = noauthcheck
                message_handler = messagehandler
              importing
                container = container
                is_ok = isok ).


  newcontdata ?= container.
  p1001 = newcontdata->pnnnn_ref( ).
  assign p1001->* to <p1001>.

* update the infotype bsed on the user decision
  <p1001>-sclas = sclas.
  <p1001>-sobid = sobid.

  newcontdata = newcontdata->modify_pnnnn( pnnnn = <p1001> ).
  newcontainer ?= newcontdata.

  updatemode-no_workflow = abap_true.
  updatemode-no_ale = abap_true.
  updatemode-translation_mode = abap_false.

  pdbuslogic->insert(
        exporting
          update_mode = updatemode
          execute_mode = c_execmodeinfotype
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          is_ok = isok
        changing
          container = newcontainer ).

  if isok eq abap_true and flush eq abap_true.
     clrhombuffer->flush( no_commit = abap_false ).
  endif.

endmethod.


method delete_relationship.
*&--------------------------------------------------------
*&  MODIFICATIONS
*&--------------------------------------------------------


* delete relats
******** i haven't included adata here yet but as its in helper it should be there eventually

  data pdbuslogic type ref to if_hrbas_infty_bl.
  data updatemode type hrbas_update_mode.
  data msg type symsg.
  data containertab type hrbas_infty_container_if_ref_t.
  data rhombuffer type ref to if_hrbas_infty_buffer.
  data clrhombuffer type ref to cl_hrbas_rhombuffer.


  DATA l_pnnnn_ref        TYPE REF TO data.
  DATA l_hrtnnnn_ref      TYPE REF TO data.
  DATA l_aux_data_ref     TYPE REF TO data.
  FIELD-SYMBOLS <pnnnn>   TYPE ANY.
  FIELD-SYMBOLS <hrtnnnn> TYPE ANY TABLE.

  DATA: l_infty_container    TYPE REF TO if_hrbas_infty_container_data,
      l_tinfty_container   TYPE REF TO if_hrbas_tinfty_container_data,
      l_is_table_infty     TYPE boole_d.
  data: ls_hrp1001 type hrp1001.
    FIELD-SYMBOLS <LS_HRIPKEY>      TYPE HRIPKEY.


  field-symbols <container> type ref to if_hrbas_infty_container.

  cl_hrbas_rhombuffer=>get_instance( importing infty_buffer = rhombuffer ).
  clrhombuffer ?= rhombuffer.
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infotype ).

* read relationships first
  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infotype
          subty = subtype
          begda = begindate
          endda = enddate
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  if containertab is initial.
*   throw error no relat exists
    msg-MSGTY = 'E'.
    msg-MSGID = 'ZHCM_MSG'.
    msg-MSGNO = '002'.
    messagehandler->add_message( exporting message = msg ).
    isok = abap_false.
    return.
  endif.

  updatemode-no_workflow = abap_true.
  updatemode-no_ale = abap_true.
  updatemode-translation_mode = abap_false.


*  read table containertab assigning <container> index 1.
  loop at containertab assigning <container>.
*      CALL METHOD pdbuslogic->convert_gui_container_to_pnnnn
*    EXPORTING
*      container    = <container>
*    IMPORTING
*      pnnnn_ref    = l_pnnnn_ref
*      hrtnnnn_ref  = l_hrtnnnn_ref
*      aux_data_ref = l_aux_data_ref.
*  ASSIGN l_pnnnn_ref->*    TO <pnnnn>.
*  ASSIGN l_hrtnnnn_ref->*  TO <hrtnnnn>.

    l_infty_container ?= <container>.
    l_pnnnn_ref   = l_infty_container->pnnnn_ref( ).

    l_is_table_infty = cl_hrbas_infotype_services=>is_table_infty(
                                         <container>->a_hripkey-infty ).
    IF l_is_table_infty = abap_true.
      l_tinfty_container ?= <container>.
      l_hrtnnnn_ref = l_tinfty_container->hrtnnnn_ref( ).

    ENDIF.

    IF l_hrtnnnn_ref IS INITIAL.
      CREATE DATA l_hrtnnnn_ref TYPE STANDARD TABLE OF hrtdbtab.
    ENDIF.

    ASSIGN l_pnnnn_ref->*    TO <pnnnn>.
    ASSIGN l_hrtnnnn_ref->*  TO <hrtnnnn>.
    ASSIGN <PNNNN> TO <LS_HRIPKEY> CASTING.
    if <LS_HRIPKEY>-begda = begindate and <LS_HRIPKEY>-endda = enddate.
      exit.
    endif.
  endloop.


  pdbuslogic->delete(
        exporting
          container = <container>
          update_mode = updatemode
          execute_mode = zif_HR_CONSTANTS_UTILITY=>c_execmodeinfotype
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          is_ok = isok ).

  if isok eq abap_true and flush eq abap_true.
    clrhombuffer->flush( no_commit = abap_false ).
  endif.

endmethod.


method get_accred_service_date.

* get accred service date
  types: begin of buffer_dir,
        sgart(2),
        client type pcl1-client,
        relid type pcl1-relid,
        srtfd type pcl1-srtfd,
        ntabx type sy-tabix,
        otabx type sy-tabix,
        nnuxt type pcl1-srtf2,
        onuxt type pcl1-srtf2,
      end of buffer_dir.

  data timeaccounts type ptm_saldo.
  data tbuff type ptt_tbuff.
  data tbuffdirect type standard table of buffer_dir.
  data payrollperiod type pabrp.
  data payrollyear type pabrj.
  data dataexists type boole_d.
  data ttspecstab type hrpad_infty_container_tab.
  data ttspecdata type ref to if_hrpa_infty_container_data.
  data p2012ref type ref to data.

  data: lv_b2_key TYPE string,
        ls_pcl2   TYPE pcl2.

  field-symbols <timeaccount> type pc2b5.
  field-symbols <ttspec> type ref to if_hrpa_infty_container.
  field-symbols <p2012> type p2012.



  CONCATENATE personnelnumber '%' INTO lv_b2_key.

* ***read PCL2 for the period to get the latest B2 Time cluster year and period
  SELECT * INTO ls_pcl2 FROM pcl2
    WHERE relid = 'B2'
      AND srtfd LIKE lv_b2_key
      AND srtf2 = 0
    ORDER BY srtfd DESCENDING.

    payrollyear   = ls_pcl2-SRTFD+8(4).
    payrollperiod = ls_pcl2-SRTFD+12(2).

    EXIT.
  ENDSELECT.


* for year and period and keydate get time eval results
* read info in time types ztart = Z101 and z102
  call function 'HR_TIME_RESULTS_GET'
    exporting
      get_pernr                   = personnelnumber
      get_pabrj                   = payrollyear
      get_pabrp                   = payrollperiod
    tables
      get_tbuff                   = tbuff
      get_buffer_dir              = tbuffdirect
      get_saldo                   = timeaccounts
   exceptions
     no_period_specified         = 1
     wrong_cluster_version       = 2
     no_read_authority           = 3
     cluster_archived            = 4
     technical_error             = 5
     others                      = 6
  .

  if sy-subrc eq 0 and timeaccounts is not initial.
    loop at timeaccounts assigning <timeaccount>.
      if <timeaccount>-ztart eq casyears.
        asyears = <timeaccount>-anzhl.
      elseif <timeaccount>-ztart eq casmonths.
        asmonths = <timeaccount>-anzhl.
      endif.
    endloop.
  endif.

*


endmethod.


method get_all_vacancies_below_org.

* read vac below org
  data structures type struc_t.
  data structurelevel type int4.
  data positionid type hrobjid.
  data p1007 type p1007.
  data p1000 type p1000.
  data messagehandler type ref to if_hrbas_message_handler.
  data isok type wdy_boolean.

  field-symbols <vacancy> type zst_positions.
  field-symbols <structure> type struc.

  create object messagehandler type cl_hrbas_message_list.

* read eval path o-o-s - all positions below org unit
  call function 'RH_STRUC_GET'
    exporting
      act_otype              = zif_HR_CONSTANTS_UTILITY=>c_otypeorgunit
      act_objid              = organizationunit
      act_wegid              = 'O-O-S'
      act_plvar              = planversion
      act_begda              = keydate
      act_endda              = keydate
      authority_check        = authcheck
    tables
      result_struc           = structures
    exceptions
      no_plvar_found         = 1
      no_entry_found         = 2
      others                 = 3.

  if sy-subrc <> 0.
    return.
  endif.

  check lines( structures ) gt 0.
  delete structures where otype ne zif_HR_CONSTANTS_UTILITY=>c_otypeposition.

  loop at structures assigning <structure>.
*   for each position check for a 1007
    positionid = <structure>-objid.

    call method ZCL_HCM_UTILITY=>read_pd_vacancy(
                          exporting
                            planversion    = planversion
                            objecttype     = ZIF_HR_CONSTANTS_UTILITY=>c_otypeposition
                            objectid       = positionid
                            begda          = keydate
                            endda          = keydate
                            messagehandler = messagehandler
                          importing
                            isok           = isok
                            p1007          = p1007 ).

    if isok eq abap_true.
      if p1007-vacan eq abap_true and p1007-status eq '0'.
*       get the stext
        call method ZCL_HCM_UTILITY=>read_pd_object(
          exporting
            objecttype     = ZIF_HR_CONSTANTS_UTILITY=>c_otypeposition
            objectid       = positionid
            begda          = keydate
            endda          = keydate
            messagehandler = messagehandler
          importing
            isok           = isok
            p1000          = p1000 ).

        check isok eq abap_true.
        append initial line to vacancies assigning <vacancy>.
        <vacancy>-plans = positionid.
        <vacancy>-stext = p1000-stext.
      endif.
    endif.

  endloop.

endmethod.


method get_chief_of_org_unit.

* get chief of org unit
* get all org units from this starting point to the top
* starting at the bottom, try to find a chief, if not go up, if yes then return

  data orgunits type struc_t.
  data managers type objec_t.

  field-symbols <orgunit> type struc.
  field-symbols <manager> type objec.

  call function 'RH_STRUC_GET'
    exporting
      act_otype              = objecttype
      act_objid              = organizationunit
      act_wegid              = evalpath
      act_plvar              = planversion
      act_begda              = keydate
      act_endda              = keydate
      authority_check        = authcheck
    tables
      result_struc           = orgunits
    exceptions
      no_plvar_found         = 1
      no_entry_found         = 2
      others                 = 3.

  if sy-subrc <> 0.
    return.
  endif.

  if lines( orgunits ) gt 0.

    if excludefirstlevel eq abap_true.
      delete orgunits index 1.
    endif.
  endif.

  loop at orgunits assigning <orgunit>.

    call function 'RH_STRUC_GET'
      exporting
        act_otype              = 'O'
        act_objid              = <orgunit>-objid
        act_wegid              = 'BOSSONLY'
        act_plvar              = planversion
        act_begda              = keydate
        act_endda              = keydate
        authority_check        = authcheck
      tables
        result_objec           = managers
      exceptions
        no_plvar_found         = 1
        no_entry_found         = 2
        others                 = 3.

    if sy-subrc <> 0.
      return.
    endif.

    loop at managers assigning <manager> where otype eq 'P'.
      managernumber = <manager>-objid.
      managername = <manager>-stext.
      return.
    endloop.

  endloop.



endmethod.


method get_chief_of_org_unit_at_level.

* get chief
* get all org units from this starting point to the top
* starting at the bottom, try to find a chief, if not go up, if yes then return
* note level = 1 is my chief, level = 2 is the 1 above the chief etc.... so base is 1 for the importing parameter

  data orgunits type struc_t.
  data managers type objec_t.
  data managerspositions type objec_t.
  data structurelevel type int4.
  data managersstruc type struc_t.

  field-symbols <recipient> type swhactor.
  field-symbols <orgunit> type struc.
  field-symbols <manager> type objec.
  field-symbols <managerstruc> type struc.

  check objecttype eq zif_HR_CONSTANTS_UTILITY=>c_otypeorgunit.

  call function 'RH_STRUC_GET'
    exporting
      act_otype              = objecttype
      act_objid              = organizationunit
      act_wegid              = evalpath
      act_plvar              = planversion
      act_begda              = keydate
      act_endda              = keydate
      authority_check        = authcheck
    tables
      result_struc           = orgunits
    exceptions
      no_plvar_found         = 1
      no_entry_found         = 2
      others                 = 3.

  if sy-subrc <> 0.
    return.
  endif.

  check lines( orgunits ) gt 0.

  structurelevel = 0.

  loop at orgunits assigning <orgunit>.
*   read chief for the org
    call function 'RH_STRUC_GET'
      exporting
        act_otype              = 'O'
        act_objid              = <orgunit>-objid
        act_wegid              = 'BOSSONLY'
        act_plvar              = planversion
        act_begda              = keydate
        act_endda              = keydate
        authority_check        = authcheck
      tables
        result_objec           = managers
        result_struc           = managersstruc
      exceptions
        no_plvar_found         = 1
        no_entry_found         = 2
        others                 = 3.

*   return type could be people or positions (since use of A/B210 has come into play
*   position to position ie S type recipients are needed
    if returnotype eq zif_HR_CONSTANTS_UTILITY=>c_otypeperson.
*     return only people - no need to worry about vacancies etc
      delete managers where otype ne zif_HR_CONSTANTS_UTILITY=>c_otypeperson.
      if managers is not initial.
        structurelevel = structurelevel + 1.
        if structurelevel eq level.
          loop at managers assigning <manager>.
            append initial line to recipients assigning <recipient>.
            <recipient>-otype = zif_HR_CONSTANTS_UTILITY=>c_otypeperson.
            <recipient>-objid = <manager>-objid.
          endloop.
          return.
        endif.
      endif.

    elseif returnotype eq zIF_HR_CONSTANTS_UTILITY=>c_otypeposition.
*     remove vacant positions so they dont become recipients
      delete managersstruc where otype eq zif_HR_CONSTANTS_UTILITY=>c_otypeposition and pdown eq '0'.
*     remove all other non position objects
      delete managersstruc where otype ne zif_HR_constants_UTILITY=>c_otypeposition.
      if managersstruc is not initial.
        structurelevel = structurelevel + 1.
        if structurelevel eq level.
          loop at managersstruc assigning <managerstruc>.
            append initial line to recipients assigning <recipient>.
            <recipient>-otype = zif_HR_constants_UTILITY=>c_otypeposition.
            <recipient>-objid = <managerstruc>-objid.
          endloop.
          return.
        endif.
      endif.
    endif.

  endloop.
endmethod.


method get_days_months_in_role.

* get role info
  types: begin of buffer_dir,
        sgart(2),
        client type pcl1-client,
        relid type pcl1-relid,
        srtfd type pcl1-srtfd,
        ntabx type sy-tabix,
        otabx type sy-tabix,
        nnuxt type pcl1-srtf2,
        onuxt type pcl1-srtf2,
      end of buffer_dir.

  field-symbols <timeaccount> type pc2b5.

  data timeaccounts type ptm_saldo.
  data tbuff type ptt_tbuff.
  data tbuffdirect type standard table of buffer_dir.

  data payrollperiod type pabrp.
  data payrollyear type pabrj.

* for payroll area get current year and period
* read t549A to get permo and
* read t549q to get pay period and year
  zcl_hcm_UTILITY=>get_payroll_year_period(
              exporting
                payrollarea = payrollarea
                keydate = keydate
              importing
                payrollyear = payrollyear
                payrollperiod = payrollperiod ).

* for year and period and keydate get time eval results
* read info in time types ztart = Z101 and z102
  call function 'HR_TIME_RESULTS_GET'
    exporting
      get_pernr                   = personnelnumber
      get_pabrj                   = payrollyear
      get_pabrp                   = payrollperiod
      get_kdate                   = keydate
    tables
      get_tbuff                   = tbuff
      get_buffer_dir              = tbuffdirect
      get_saldo                   = timeaccounts
   exceptions
     no_period_specified         = 1
     wrong_cluster_version       = 2
     no_read_authority           = 3
     cluster_archived            = 4
     technical_error             = 5
     others                      = 6
  .

  if sy-subrc eq 0 and timeaccounts is not initial.
    loop at timeaccounts assigning <timeaccount>.
      if <timeaccount>-ztart eq cdaysinrole.
        daysinrole = <timeaccount>-anzhl.
      elseif <timeaccount>-ztart eq cmonthsinrole.
        monthsinrole = <timeaccount>-anzhl.
      endif.
    endloop.
  endif.



endmethod.


method get_email_address.

* get ee email
  data p0105 type p0105.
  data dataexists type wdy_boolean.

  call method zcl_hcm_UTILITY=>read_infotype(
      exporting
        begindate	= begindate
        enddate	= begindate
        personnelnumber	= personnelnumber
        infotype  = '0105'
        subtype	= '0010'
      importing
        pnnnn	= p0105
        dataexists = dataexists ).

  if dataexists eq abap_true.
    emailaddress = p0105-usrid_long.
  else.
    raise exception type zcx_no_data_exists.
  endif.

endmethod.


METHOD get_employee_know_as_name.
*-----------------------------------------------------------------------------
* 2018-02-16 BFung 5000010295
* - get know as name if available
*-----------------------------------------------------------------------------

  DATA p0002 TYPE p0002.
  DATA dataexists TYPE wdy_boolean.

  CALL METHOD zcl_hcm_UTILITY=>read_infotype(
    EXPORTING
      begindate       = begindate
      enddate         = begindate
      personnelnumber = personnelnumber
      infotype        = '0002'
    IMPORTING
      pnnnn           = p0002
      dataexists      = dataexists ).

  IF dataexists EQ abap_true.
    IF p0002-rufnm IS INITIAL.
      CONCATENATE p0002-vorna p0002-nachn INTO employeename SEPARATED BY space.
    ELSE.
      CONCATENATE p0002-rufnm p0002-nachn INTO employeename SEPARATED BY space.
    ENDIF.
  ELSE.
    CLEAR employeename.
    RAISE EXCEPTION TYPE zcx_no_data_exists.
  ENDIF.

ENDMETHOD.


method get_employee_name.

* get ee name
  data p0001 type p0001.
  data dataexists type wdy_boolean.

  call method zcl_hcm_UTILITY=>read_infotype(
      exporting
        begindate	= begindate
        enddate	= begindate
        personnelnumber	= personnelnumber
        infotype  = '0001'
      importing
        pnnnn	= p0001
        dataexists = dataexists ).

  if dataexists eq abap_true.
    employeename = p0001-ename.
  else.
    clear employeename.
    raise exception type zcx_no_data_exists.
  endif.

endmethod.


method GET_MANAGER_FOR_EMPLOYEE.

* get my manager
  data p0001 type p0001.
  data dataexists type boole_d.
  data orgunitid type hrobjid.
  data excludefirstlevel type boole_d.

* get the manager for the employee
* read infotype 1
  call method zcl_hcm_UTILITY=>read_infotype
    exporting
      begindate       = keydate
      enddate         = keydate
      personnelnumber = personnelnumber
      infotype        = '0001'
    importing
      pnnnn           = p0001
      dataexists      = dataexists.

  check dataexists eq abap_true or p0001-orgeh is not initial.

  orgunitid = p0001-orgeh.

* check if this employee is actually the chief of this org unit
* if so we will need the next higher chief to be returned
  excludefirstlevel = zcl_hcm_UTILITY=>is_employee_a_manager( personnelnumber  = personnelnumber keydate = keydate ).

  call method zcl_hcm_UTILITY=>get_chief_of_org_unit
    exporting
      organizationunit   = orgunitid
       keydate           = keydate
       excludefirstlevel = excludefirstlevel
    importing
      managernumber      = managernumber
      managername        = managername.


endmethod.


method get_object_short_text.

* get obj short
  select single short from hrp1000 into shorttext
    where
        plvar	eq planversion and
        otype	eq objecttype and
        objid	eq objectid and
        istat	eq status and
        begda	le keydate and
        endda	ge keydate and
        langu	eq sy-langu.

endmethod.


METHOD get_orgunit_of_position.

* get the parent org unit of position

  DATA orgunits TYPE struc_t.

  FIELD-SYMBOLS <orgunit> TYPE struc.

  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype       = zif_hr_constants_utility=>c_otypeposition
      act_objid       = positionid
      act_wegid       = 'A003'
      act_plvar       = zif_hr_constants_utility=>c_planversion01
      act_begda       = keydate
      act_endda       = keydate
      authority_check = authcheck
    TABLES
      result_struc    = orgunits
    EXCEPTIONS
      no_plvar_found  = 1
      no_entry_found  = 2
      OTHERS          = 3.

  DELETE orgunits WHERE otype NE zif_HR_constants_UTILITY=>c_otypeorgunit.

  READ TABLE orgunits ASSIGNING <orgunit> INDEX 1.
  IF sy-subrc EQ 0.
    orgunitid = <orgunit>-objid.
  ENDIF.

ENDMETHOD.


method GET_PAYROLL_PERIOD_BEGIN_DATES.

* get pr dates
  data permo type permo.
  data ls_t549a type t549a.
  data ls_t549q type t549q.

  ls_t549a = cl_hr_t549a=>read( abkrs = payrollarea ).
  permo = ls_t549a-permo.

  ls_t549q = cl_hr_t549q=>read_first_permo_at_date( permo = permo begda = keydate ).

  if ls_t549q-begda is initial or ls_t549q-endda is initial.
    isok = abap_false.
  else.
    isok = abap_true.
    currentperiodbegin = ls_t549q-begda.
    nextperiodbegin = ls_t549q-endda + 1.
  endif.

endmethod.


method get_payroll_year_period.

* get payroll info
  data permo type permo.
  data isok type boole_d.
  data ls_t549a type t549a.
  data ls_t549q type t549q.

  ls_t549a = cl_hr_t549a=>read( abkrs = payrollarea ).
  permo = ls_t549a-permo.

  ls_t549q = cl_hr_t549q=>read_first_permo_at_date( permo = permo begda = keydate ).

  payrollperiod = ls_t549q-pabrp.
  payrollyear = ls_t549q-pabrj.

endmethod.


method get_personnel_number.

* get pernr
  select single pernr from pa0105 into personnelnumber
        where begda <= begindate and endda >= begindate and usrid = username.

  if sy-subrc ne 0.
    clear personnelnumber.
    raise exception type zcx_no_data_exists.
  endif.

endmethod.


method GET_POSITION_HOLDERS.

* get pos holders
  call function 'RH_STRUC_GET'
    exporting
      act_otype       = zif_hr_constants_utility=>c_otypeposition
      act_objid       = positionid
      act_wegid       = 'A008'
      act_plvar       = zif_hr_constants_utility=>c_planversion01
      act_begda       = keydate
      act_endda       = keydate
      authority_check = authcheck
    tables
      result_tab      = holders
    exceptions
      no_plvar_found  = 1
      no_entry_found  = 2
      others          = 3.


  if ( sy-subrc ne 0 or positionid is initial ) and sy-subrc <> 2.
    raise exception type zcx_no_data_exists.
    return.
  endif.

  delete holders where otype ne zif_hr_constants_utility=>c_otypeperson.

endmethod.


method GET_POSITION_OF_EMPLOYEE.

* get pos of ee
  data positions type struc_t.
  field-symbols <position> type struc.

  call function 'RH_STRUC_GET'
    exporting
      act_otype              = objecttype
      act_objid              = personnelnumber
      act_wegid              = evalpath
      act_plvar              = planversion
      act_begda              = keydate
      act_endda              = keydate
      authority_check        = authcheck
    tables
      result_struc           = positions
    exceptions
      no_plvar_found         = 1
      no_entry_found         = 2
      others                 = 3.

  if sy-subrc ne 0.
    raise exception type zcx_no_data_exists.
  endif.

  loop at positions assigning <position> where otype eq 'S'.
    positionid = <position>-objid.
    return.
  endloop.


endmethod.


method GET_USERNAME_FOR_EMPLOYEE.

* get username for ee
  select single usrid into username from pa0105 where pernr eq personnelnumber and subty eq '0001' and begda <= date and endda >= date.

  if sy-subrc ne 0.
    raise exception type zcx_no_data_exists.
  endif.

endmethod.


METHOD is_employee.
*-----------------------------------------------------------------------------
* Modification Log
* Date       Developer/Comments
*-----------------------------------------------------------------------------
* 2019-10-15 BFung 5000013352
* - Determine employee
*-----------------------------------------------------------------------------
  DATA: lv_pernr TYPE persno,
        lv_persg TYPE persg.

  r_is_employee = abap_false.

* ***get pernr from system
  IF i_pernr IS INITIAL AND i_usrid IS INITIAL.
    SELECT pernr INTO lv_pernr
      FROM pa0105 UP TO 1 ROWS
      WHERE usrty = '0001'
        AND usrid = sy-uname.
    ENDSELECT.

*   ***not employee
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ELSE.
*   ***get pernr from user id
    IF i_usrid IS NOT INITIAL.
      SELECT pernr INTO lv_pernr
        FROM pa0105 UP TO 1 ROWS
        WHERE usrty = '0001'
          AND usrid = i_usrid.
      ENDSELECT.

*     ***not employee
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ELSE.
      lv_pernr = i_pernr.
    ENDIF.
  ENDIF.

* ***is employee?
  SELECT persg INTO lv_persg
    FROM pa0001 UP TO 1 ROWS
    WHERE pernr = lv_pernr
      AND begda <= sy-datum
      AND endda >= sy-datum.

    IF lv_persg = '1'.
      r_is_employee = abap_true.
    ENDIF.
  ENDSELECT.

  RETURN.

ENDMETHOD.


method IS_EMPLOYEE_A_MANAGER.

* is ee a manager
* get all org units from this starting point to the top
* starting at the bottom, try to find a chief, if not go up, if yes then return

  data orgunits type struc_t.
  field-symbols <orgunit> type struc.

  call function 'RH_STRUC_GET'
    exporting
      act_otype              = objecttype
      act_objid              = personnelnumber
      act_wegid              = evalpath
      act_plvar              = planversion
      act_begda              = keydate
      act_endda              = keydate
      authority_check        = authcheck
    tables
      result_struc           = orgunits
    exceptions
      no_plvar_found         = 1
      no_entry_found         = 2
      others                 = 3.

  if sy-subrc <> 0.
    ismanager = abap_false.
    return.
  endif.

  loop at orgunits assigning <orgunit> where otype eq 'O'.
    ismanager = abap_true.
    return.
  endloop.



endmethod.


method IS_POSITION_A_MANAGER.

* is pos a manager pos
  data orgunits type struc_t.
  field-symbols <orgunit> type struc.

  call function 'RH_STRUC_GET'
    exporting
      act_otype              = objecttype
      act_objid              = positionid
      act_wegid              = evalpath
      act_plvar              = planversion
      act_begda              = keydate
      act_endda              = keydate
      authority_check        = authcheck
    tables
      result_struc           = orgunits
    exceptions
      no_plvar_found         = 1
      no_entry_found         = 2
      others                 = 3.

  loop at orgunits assigning <orgunit> where otype eq 'O'.
    ismanager = abap_true.
    return.
  endloop.



endmethod.


method is_position_occupied.

* is position occupied
  data holders type struc_t.

  call function 'RH_STRUC_GET'
    exporting
      act_otype       = zif_hr_constants_utility=>c_otypeposition
      act_objid       = positionid
      act_wegid       = 'A008'
      act_plvar       = zif_hr_constants_utility=>c_planversion01
      act_begda       = keydate
      act_endda       = keydate
      authority_check = authcheck
    tables
      result_struc    = holders
    exceptions
      no_plvar_found  = 1
      no_entry_found  = 2
      others          = 3.

  delete holders where otype ne zif_hr_constants_utility=>c_otypeperson.

  if lines( holders ) gt 0.
    hasholder = abap_true.
  else.
    hasholder = abap_false.
  endif.

endmethod.


method READ_EEGROUP_SUBGROUP.

* read ee group subgroup
  data pdbuslogic type ref to if_hrbas_infty_bl.
  data containertab type hrbas_infty_container_if_ref_t.
  data pnnnn type ref to data.
  data containerdata type ref to if_hrbas_infty_container_data.

  field-symbols <pnnnn> type any.
  field-symbols <container> type ref to if_hrbas_infty_container.

  create data pnnnn type p1013.

  check infty eq '1013'.

* read PD infotype - single!
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infty ).

  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infty
          subty = subty
          begda = begda
          endda = endda
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  if containertab is not initial.
    read table containertab assigning <container> index 1.
    containerdata ?= <container>.
    pnnnn = containerdata->pnnnn_ref( ).
    assign pnnnn->* to <pnnnn>.
    p1013 = <pnnnn>.
    isok = abap_true.
  else.
    isok = abap_false.
  endif.


endmethod.


method read_infotype.

* read an infotype
  data masterdatadb type ref to if_hrpa_masterdata_db.
  data infotypereader type ref to if_hrpa_read_infotype.
  data masterdatabuffer type ref to if_hrpa_masterdata_buffer.

  try.
    cl_hrpa_masterdata_db=>get_instance( importing masterdata_db = masterdatadb ).
    cl_hrpa_masterdata_buffer=>get_instance( exporting masterdata_db = masterdatadb importing masterdata_buffer = masterdatabuffer ).
    cl_hrpa_read_infotype=>get_instance( exporting masterdata_buffer = masterdatabuffer importing infotype_reader = infotypereader ).

    infotypereader->read_single(
                      exporting
                        tclas	= tclas
                        pernr	= personnelnumber
                        infty	= infotype
                        subty	= subtype
                        objps	= objps
                        sprps	= sprps
                        begda	= begindate
                        endda	= enddate
                        mode  = mode
                        no_auth_check	= noauthcheck
                      importing
                        pnnnn	= pnnnn
                        data_exists	= dataexists ).


   catch cx_hrpa_violated_assertion.
     dataexists = abap_false.

  endtry.

endmethod.


method read_infotype_tc3.

* read time constraint 3
  data hrpabuffer type ref to if_hrpa_masterdata_buffer.
  data hrpadb type ref to if_hrpa_masterdata_db.
  data itbl type ref to if_hrpa_infty_bl.
  data itcontainer type ref to if_hrpa_infty_container.
  data containerdata type ref to if_hrpa_infty_container_data.
  data messagehandler type ref to if_hrpa_message_handler.
  data isok type wdy_boolean.

  create object messagehandler type cl_hrpa_message_list.

* get inastance of masterdatabuffer db
  cl_hrpa_masterdata_db=>get_instance( importing masterdata_db = hrpadb ).
* get instance of masterdatabuffer
  cl_hrpa_masterdata_buffer=>get_instance( exporting masterdata_db = hrpadb importing masterdata_buffer = hrpabuffer ).

* get infotype business logic
  try.
      cl_hrpa_infotype_factory=>get_specific_infotype_logic(
                exporting
                  tclas	= tclas
                  infty	= infotype
                  versionid	= infotypeversion
                importing
                  infotype_logic  = itbl ).
    catch cx_hrpa_violated_assertion.
      dataexists = abap_false.
      return.
  endtry.

  try.
      itbl->read(
        exporting
          tclas	= tclas
          pernr	= personnelnumber
          infty	= infotype
          subty	= subtype
          objps	= objps
          sprps	= sprps
          begda	= begindate
          endda	= enddate
          no_auth_check	= noauthcheck
          message_handler	= messagehandler
        importing
          container_tab = infotypetab
          is_ok	= isok ).

    catch cx_hrpa_violated_assertion.
      dataexists = abap_false.
      return.
  endtry.

  if infotypetab is not initial.
    dataexists = abap_true.
  endif.

endmethod.


method READ_PD_ENT_STRUCTURE.

* read pd ent struc
  data pdbuslogic type ref to if_hrbas_infty_bl.
  data containertab type hrbas_infty_container_if_ref_t.
  data pnnnn type ref to data.
  data containerdata type ref to if_hrbas_infty_container_data.

  field-symbols <pnnnn> type any.
  field-symbols <container> type ref to if_hrbas_infty_container.

  create data pnnnn type p1008.

  check infty eq '1008'.

* read PD infotype - single!
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infty ).

  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infty
          subty = subty
          begda = begda
          endda = endda
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  if containertab is not initial.
    read table containertab assigning <container> index 1.
    containerdata ?= <container>.
    pnnnn = containerdata->pnnnn_ref( ).
    assign pnnnn->* to <pnnnn>.
    p1008 = <pnnnn>.
    isok = abap_true.
  else.
    isok = abap_false.
  endif.


endmethod.


method READ_PD_INFOTYPE_SINGLE.

* read pd single
  data pdbuslogic type ref to if_hrbas_infty_bl.
  data containertab type hrbas_infty_container_if_ref_t.
  data pdatnnnn type ref to data.
  data containerdata type ref to if_hrbas_infty_container_data.

  field-symbols <pnnnn> type any.
  field-symbols <container> type ref to if_hrbas_infty_container.

* read PD infotype - single!
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infty ).

  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infty
          subty = subty
          begda = begda
          endda = endda
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  if containertab is not initial.
    read table containertab assigning <container> index 1.
    containerdata ?= <container>.
    pdatnnnn = containerdata->pnnnn_ref( ).
    assign pdatnnnn->* to <pnnnn>.
    pnnnn = <pnnnn>.
    isok = abap_true.
  else.
    isok = abap_false.
  endif.

endmethod.


method READ_PD_OBJECT.

* read pd object
  data pdbuslogic type ref to if_hrbas_infty_bl.
  data containertab type hrbas_infty_container_if_ref_t.
  data pnnnn type ref to data.
  data containerdata type ref to if_hrbas_infty_container_data.

  field-symbols <pnnnn> type any.
  field-symbols <container> type ref to if_hrbas_infty_container.

  create data pnnnn type p1000.

  check infty eq '1000'.

* read PD infotype - single!
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infty ).

  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infty
          begda = begda
          endda = endda
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  if containertab is not initial.
    read table containertab assigning <container> index 1.
    containerdata ?= <container>.
    pnnnn = containerdata->pnnnn_ref( ).
    assign pnnnn->* to <pnnnn>.
    p1000 = <pnnnn>.
    isok = abap_true.
  else.
    isok = abap_false.
  endif.


endmethod.


method read_pd_planned_comp_single.

* read planned comp single
  data pdbuslogic type ref to if_hrbas_infty_bl.
  data containertab type hrbas_infty_container_if_ref_t.
  data pnnnn type ref to data.
  data containerdata type ref to if_hrbas_infty_container_data.

  field-symbols <pnnnn> type any.
  field-symbols <container> type ref to if_hrbas_infty_container.

  create data pnnnn type p1005.

  check infty eq '1005'.

* read PD infotype - single!
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infty ).

  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infty
          subty = subty
          begda = begda
          endda = endda
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  if containertab is not initial.
    read table containertab assigning <container> index 1.
    containerdata ?= <container>.
    pnnnn = containerdata->pnnnn_ref( ).
    assign pnnnn->* to <pnnnn>.
    p1005 = <pnnnn>.
    isok = abap_true.
  else.
    isok = abap_false.
  endif.


endmethod.


method READ_PD_VACANCY.

* read pd vacancy
  data pdbuslogic type ref to if_hrbas_infty_bl.
  data containertab type hrbas_infty_container_if_ref_t.
  data pnnnn type ref to data.
  data containerdata type ref to if_hrbas_infty_container_data.

  field-symbols <pnnnn> type any.
  field-symbols <container> type ref to if_hrbas_infty_container.

  create data pnnnn type p1007.

  check infty eq '1007'.

* read PD infotype - single!
  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infty ).

  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infty
          subty = subty
          begda = begda
          endda = endda
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  if containertab is not initial.
    read table containertab assigning <container> index 1.
    containerdata ?= <container>.
    pnnnn = containerdata->pnnnn_ref( ).
    assign pnnnn->* to <pnnnn>.
    p1007 = <pnnnn>.
    isok = abap_true.
  else.
    isok = abap_false.
  endif.


endmethod.


method read_relationship_infoptypes.

* read relats
  data pdbuslogic type ref to if_hrbas_infty_bl.
  data isok type boole_d.
  data containertab type hrbas_infty_container_if_ref_t.
  data p1001 type ref to data.
  data containerdata type ref to if_hrbas_infty_container_data.

  field-symbols <p1001> type p1001.
  field-symbols <container> type ref to if_hrbas_infty_container.

  check infty eq '1001'.

  pdbuslogic = cl_hrbas_infotype_factory=>get_infotype_logic( infty = infty ).

  pdbuslogic->read(
        exporting
          plvar = planversion
          otype = objecttype
          objid = objectid
          istat = status
          infty = infty
          subty = subty
          begda = begda
          endda = endda
          no_auth_check = noauthcheck
          message_handler = messagehandler
        importing
          container_tab = containertab
          is_ok = isok ).

  loop at containertab assigning <container>.
    containerdata ?= <container>.
    p1001 = containerdata->pnnnn_ref( ).
    assign p1001->* to <p1001>.
    append <p1001> to relationships.
  endloop.

endmethod.
ENDCLASS.
