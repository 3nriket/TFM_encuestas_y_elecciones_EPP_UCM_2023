/* --------------------------------------------------------------------
  Código para la importación de los datos
   -------------------------------------------------------------------- */
DATA WORK.train_semma;
    LENGTH
        year_elec          8
        n_days_field       8
        days_to_elec       8
        porc_surveys_firm   8
        n                  8
        exit_poll        $ 5
        est_surv_vote      8
        prom_general_partido   8
        prom_general_wing   8
        prom_casa_partido   8
        prom_casa_wing     8
        prom_carrera_partido   8
        prom_carrera_wing   8
        prom_carrera_casa_partido   8
        prom_carrera_casa_wing   8
        house_effect_e     8
        wing_effect_e      8
        urna_0             8
        urna_7             8
        urna_15            8
        urna_60            8
        urna_365           8
        pobl_densidad      8
        pobl_fem_porc      8
        pobl               8
        pobl_kill          8
        pobl_kill_percienmil   8
        pobl_suicide       8
        pobl_suicide_percienmil   8
        pobl_life_expectancy   8
        pobl_idh           8
        pobl_im_rate       8
        pobl_em_rate       8
        pobl_pobreza_rate   8
        eco_smi            8
        eco_rate_avg       8
        eco_fisc_ing       8
        eco_fisc_ing_percap   8
        eco_debt_percap    8
        eco_deficit        8
        eco_pib_var        8
        env_gwh_prod       8
        env_gwh_prod_renovable   8
        env_gwh_consum     8
        env_kwh_consum_percap   8
        env_co2            8
        env_co2_percap     8
        eco_unployement    8
        eco_pib_percap     8
        gov_exp_pib        8
        gov_cor_rate       8
        gov_exp_war        8
        gov_exp_war_percap   8
        gov_exp_san        8
        gov_exp_san_percap   8
        gov_exp_edu        8
        gov_exp_edu_percap   8
        errores            8
        party_AP           8
        party_BNG          8
        party_CC           8
        'party_CC.NC'n     8
        party_CCC          8
        party_CDS          8
        party_CIU          8
        party_CS           8
        party_CUP          8
        party_EA           8
        party_EE           8
        'party_EH.BILDU'n   8
        party_ERC          8
        party_EV           8
        party_FN           8
        party_HB           8
        party_IU           8
        party_JC           8
        party_MP           8
        party_NS           8
        party_PA           8
        party_PCE          8
        party_PNV          8
        party_PODEMOS      8
        party_PP           8
        party_PRC          8
        party_PSOE         8
        party_UCD          8
        party_UP           8
        party_UPYD         8
        party_VOX          8
        wing_LEFT          8
        wing_RIGHT         8
        poll_firm_ASEP     8
        'poll_firm_CELESTE.TEL'n   8
        poll_firm_CIS      8
        poll_firm_DYM      8
        poll_firm_ELECTOPANEL   8
        poll_firm_GAD3     8
        poll_firm_GALLUP   8
        poll_firm_GESOP    8
        'poll_firm_HAMALGAMA_MÃ‰TRICA'n   8
        poll_firm_IMOP     8
        poll_firm_METROSCOPIA   8
        poll_firm_MYWORD   8
        poll_firm_NC_REPORT   8
        poll_firm_NOXA     8
        poll_firm_OBRADOIRO_SOCIO   8
        poll_firm_OPINA    8
        poll_firm_SIGMA_DOS   8
        'poll_firm_SIMPLE_LÃ“GICA'n   8
        'poll_firm_SOCIOMÃ‰TRICA'n   8
        poll_firm_TNS_DEMOSCOPIA   8
        'poll_firm_VOX_PÃšBLICA'n   8
        lead_party_CS      8
        lead_party_PODEMOS   8
        lead_party_PP      8
        lead_party_PSOE    8
        lead_party_UCD     8
        lead2_party_AP     8
        lead2_party_ARM    8
        lead2_party_CS     8
        lead2_party_EA     8
        lead2_party_PODEMOS   8
        lead2_party_PP     8
        lead2_party_PSOE   8
        lead2_party_UCD    8
        lead2_party_UP     8
        lead2_party_VOX    8
        gov_pre_PP         8
        gov_pre_PSOE       8
        gov_pre_UCD        8 ;
    DROP
        error_general_partido
        error_general_wing
        error_casa_partido
        error_casa_wing
        error_carrera_partido
        error_carrera_wing
        error_carrera_casa_partido
        error_carrera_casa_wing
        house_effect
        wing_effect ;
    FORMAT
        year_elec        BEST4.
        n_days_field     BEST2.
        days_to_elec     BEST4.
        porc_surveys_firm BEST18.
        n                BEST6.
        exit_poll        $CHAR5.
        est_surv_vote    BEST4.
        prom_general_partido BEST4.
        prom_general_wing BEST4.
        prom_casa_partido BEST4.
        prom_casa_wing   BEST4.
        prom_carrera_partido BEST4.
        prom_carrera_wing BEST4.
        prom_carrera_casa_partido BEST4.
        prom_carrera_casa_wing BEST4.
        house_effect_e   BEST20.
        wing_effect_e    BEST20.
        urna_0           BEST1.
        urna_7           BEST1.
        urna_15          BEST1.
        urna_60          BEST1.
        urna_365         BEST1.
        pobl_densidad    BEST2.
        pobl_fem_porc    BEST18.
        pobl             BEST8.
        pobl_kill        BEST17.
        pobl_kill_percienmil BEST18.
        pobl_suicide     BEST4.
        pobl_suicide_percienmil BEST4.
        pobl_life_expectancy BEST5.
        pobl_idh         BEST18.
        pobl_im_rate     BEST20.
        pobl_em_rate     BEST20.
        pobl_pobreza_rate BEST19.
        eco_smi          BEST16.
        eco_rate_avg     BEST5.
        eco_fisc_ing     BEST17.
        eco_fisc_ing_percap BEST17.
        eco_debt_percap  BEST5.
        eco_deficit      BEST7.
        eco_pib_var      BEST6.
        env_gwh_prod     BEST6.
        env_gwh_prod_renovable BEST6.
        env_gwh_consum   BEST6.
        env_kwh_consum_percap BEST6.
        env_co2          BEST7.
        env_co2_percap   BEST4.
        eco_unployement  BEST5.
        eco_pib_percap   BEST5.
        gov_exp_pib      BEST6.
        gov_cor_rate     BEST18.
        gov_exp_war      BEST7.
        gov_exp_war_percap BEST3.
        gov_exp_san      BEST7.
        gov_exp_san_percap BEST4.
        gov_exp_edu      BEST17.
        gov_exp_edu_percap BEST17.
        errores          BEST20.
        party_AP         BEST1.
        party_BNG        BEST1.
        party_CC         BEST1.
        'party_CC.NC'n   BEST1.
        party_CCC        BEST1.
        party_CDS        BEST1.
        party_CIU        BEST1.
        party_CS         BEST1.
        party_CUP        BEST1.
        party_EA         BEST1.
        party_EE         BEST1.
        'party_EH.BILDU'n BEST1.
        party_ERC        BEST1.
        party_EV         BEST1.
        party_FN         BEST1.
        party_HB         BEST1.
        party_IU         BEST1.
        party_JC         BEST1.
        party_MP         BEST1.
        party_NS         BEST1.
        party_PA         BEST1.
        party_PCE        BEST1.
        party_PNV        BEST1.
        party_PODEMOS    BEST1.
        party_PP         BEST1.
        party_PRC        BEST1.
        party_PSOE       BEST1.
        party_UCD        BEST1.
        party_UP         BEST1.
        party_UPYD       BEST1.
        party_VOX        BEST1.
        wing_LEFT        BEST1.
        wing_RIGHT       BEST1.
        poll_firm_ASEP   BEST1.
        'poll_firm_CELESTE.TEL'n BEST1.
        poll_firm_CIS    BEST1.
        poll_firm_DYM    BEST1.
        poll_firm_ELECTOPANEL BEST1.
        poll_firm_GAD3   BEST1.
        poll_firm_GALLUP BEST1.
        poll_firm_GESOP  BEST1.
        'poll_firm_HAMALGAMA_MÃ‰TRICA'n BEST1.
        poll_firm_IMOP   BEST1.
        poll_firm_METROSCOPIA BEST1.
        poll_firm_MYWORD BEST1.
        poll_firm_NC_REPORT BEST1.
        poll_firm_NOXA   BEST1.
        poll_firm_OBRADOIRO_SOCIO BEST1.
        poll_firm_OPINA  BEST1.
        poll_firm_SIGMA_DOS BEST1.
        'poll_firm_SIMPLE_LÃ“GICA'n BEST1.
        'poll_firm_SOCIOMÃ‰TRICA'n BEST1.
        poll_firm_TNS_DEMOSCOPIA BEST1.
        'poll_firm_VOX_PÃšBLICA'n BEST1.
        lead_party_CS    BEST1.
        lead_party_PODEMOS BEST1.
        lead_party_PP    BEST1.
        lead_party_PSOE  BEST1.
        lead_party_UCD   BEST1.
        lead2_party_AP   BEST1.
        lead2_party_ARM  BEST1.
        lead2_party_CS   BEST1.
        lead2_party_EA   BEST1.
        lead2_party_PODEMOS BEST1.
        lead2_party_PP   BEST1.
        lead2_party_PSOE BEST1.
        lead2_party_UCD  BEST1.
        lead2_party_UP   BEST1.
        lead2_party_VOX  BEST1.
        gov_pre_PP       BEST1.
        gov_pre_PSOE     BEST1.
        gov_pre_UCD      BEST1. ;
    INFORMAT
        year_elec        BEST4.
        n_days_field     BEST2.
        days_to_elec     BEST4.
        porc_surveys_firm BEST18.
        n                BEST6.
        exit_poll        $CHAR5.
        est_surv_vote    BEST4.
        prom_general_partido BEST4.
        prom_general_wing BEST4.
        prom_casa_partido BEST4.
        prom_casa_wing   BEST4.
        prom_carrera_partido BEST4.
        prom_carrera_wing BEST4.
        prom_carrera_casa_partido BEST4.
        prom_carrera_casa_wing BEST4.
        house_effect_e   BEST20.
        wing_effect_e    BEST20.
        urna_0           BEST1.
        urna_7           BEST1.
        urna_15          BEST1.
        urna_60          BEST1.
        urna_365         BEST1.
        pobl_densidad    BEST2.
        pobl_fem_porc    BEST18.
        pobl             BEST8.
        pobl_kill        BEST17.
        pobl_kill_percienmil BEST18.
        pobl_suicide     BEST4.
        pobl_suicide_percienmil BEST4.
        pobl_life_expectancy BEST5.
        pobl_idh         BEST18.
        pobl_im_rate     BEST20.
        pobl_em_rate     BEST20.
        pobl_pobreza_rate BEST19.
        eco_smi          BEST16.
        eco_rate_avg     BEST5.
        eco_fisc_ing     BEST17.
        eco_fisc_ing_percap BEST17.
        eco_debt_percap  BEST5.
        eco_deficit      BEST7.
        eco_pib_var      BEST6.
        env_gwh_prod     BEST6.
        env_gwh_prod_renovable BEST6.
        env_gwh_consum   BEST6.
        env_kwh_consum_percap BEST6.
        env_co2          BEST7.
        env_co2_percap   BEST4.
        eco_unployement  BEST5.
        eco_pib_percap   BEST5.
        gov_exp_pib      BEST6.
        gov_cor_rate     BEST18.
        gov_exp_war      BEST7.
        gov_exp_war_percap BEST3.
        gov_exp_san      BEST7.
        gov_exp_san_percap BEST4.
        gov_exp_edu      BEST17.
        gov_exp_edu_percap BEST17.
        errores          BEST20.
        party_AP         BEST1.
        party_BNG        BEST1.
        party_CC         BEST1.
        'party_CC.NC'n   BEST1.
        party_CCC        BEST1.
        party_CDS        BEST1.
        party_CIU        BEST1.
        party_CS         BEST1.
        party_CUP        BEST1.
        party_EA         BEST1.
        party_EE         BEST1.
        'party_EH.BILDU'n BEST1.
        party_ERC        BEST1.
        party_EV         BEST1.
        party_FN         BEST1.
        party_HB         BEST1.
        party_IU         BEST1.
        party_JC         BEST1.
        party_MP         BEST1.
        party_NS         BEST1.
        party_PA         BEST1.
        party_PCE        BEST1.
        party_PNV        BEST1.
        party_PODEMOS    BEST1.
        party_PP         BEST1.
        party_PRC        BEST1.
        party_PSOE       BEST1.
        party_UCD        BEST1.
        party_UP         BEST1.
        party_UPYD       BEST1.
        party_VOX        BEST1.
        wing_LEFT        BEST1.
        wing_RIGHT       BEST1.
        poll_firm_ASEP   BEST1.
        'poll_firm_CELESTE.TEL'n BEST1.
        poll_firm_CIS    BEST1.
        poll_firm_DYM    BEST1.
        poll_firm_ELECTOPANEL BEST1.
        poll_firm_GAD3   BEST1.
        poll_firm_GALLUP BEST1.
        poll_firm_GESOP  BEST1.
        'poll_firm_HAMALGAMA_MÃ‰TRICA'n BEST1.
        poll_firm_IMOP   BEST1.
        poll_firm_METROSCOPIA BEST1.
        poll_firm_MYWORD BEST1.
        poll_firm_NC_REPORT BEST1.
        poll_firm_NOXA   BEST1.
        poll_firm_OBRADOIRO_SOCIO BEST1.
        poll_firm_OPINA  BEST1.
        poll_firm_SIGMA_DOS BEST1.
        'poll_firm_SIMPLE_LÃ“GICA'n BEST1.
        'poll_firm_SOCIOMÃ‰TRICA'n BEST1.
        poll_firm_TNS_DEMOSCOPIA BEST1.
        'poll_firm_VOX_PÃšBLICA'n BEST1.
        lead_party_CS    BEST1.
        lead_party_PODEMOS BEST1.
        lead_party_PP    BEST1.
        lead_party_PSOE  BEST1.
        lead_party_UCD   BEST1.
        lead2_party_AP   BEST1.
        lead2_party_ARM  BEST1.
        lead2_party_CS   BEST1.
        lead2_party_EA   BEST1.
        lead2_party_PODEMOS BEST1.
        lead2_party_PP   BEST1.
        lead2_party_PSOE BEST1.
        lead2_party_UCD  BEST1.
        lead2_party_UP   BEST1.
        lead2_party_VOX  BEST1.
        gov_pre_PP       BEST1.
        gov_pre_PSOE     BEST1.
        gov_pre_UCD      BEST1. ;
    INFILE DATALINES4
        DLM='7F'x
        MISSOVER
        DSD ;
    INPUT
        year_elec        : ?? BEST4.
        n_days_field     : ?? BEST2.
        days_to_elec     : ?? BEST4.
        porc_surveys_firm : ?? COMMA18.
        n                : ?? BEST6.
        exit_poll        : $CHAR5.
        est_surv_vote    : ?? COMMA4.
        prom_general_partido : ?? COMMA4.
        error_general_partido : $1.
        prom_general_wing : ?? COMMA4.
        error_general_wing : $1.
        prom_casa_partido : ?? COMMA4.
        error_casa_partido : $1.
        prom_casa_wing   : ?? COMMA4.
        error_casa_wing  : $1.
        prom_carrera_partido : ?? COMMA4.
        error_carrera_partido : $1.
        prom_carrera_wing : ?? COMMA4.
        error_carrera_wing : $1.
        prom_carrera_casa_partido : ?? COMMA4.
        error_carrera_casa_partido : $1.
        prom_carrera_casa_wing : ?? COMMA4.
        error_carrera_casa_wing : $1.
        house_effect_e   : ?? COMMA20.
        house_effect     : $1.
        wing_effect_e    : ?? COMMA20.
        wing_effect      : $1.
        urna_0           : ?? BEST1.
        urna_7           : ?? BEST1.
        urna_15          : ?? BEST1.
        urna_60          : ?? BEST1.
        urna_365         : ?? BEST1.
        pobl_densidad    : ?? BEST2.
        pobl_fem_porc    : ?? COMMA18.
        pobl             : ?? BEST8.
        pobl_kill        : ?? COMMA17.
        pobl_kill_percienmil : ?? COMMA18.
        pobl_suicide     : ?? BEST4.
        pobl_suicide_percienmil : ?? COMMA4.
        pobl_life_expectancy : ?? COMMA5.
        pobl_idh         : ?? COMMA18.
        pobl_im_rate     : ?? COMMA20.
        pobl_em_rate     : ?? COMMA20.
        pobl_pobreza_rate : ?? COMMA19.
        eco_smi          : ?? COMMA16.
        eco_rate_avg     : ?? BEST5.
        eco_fisc_ing     : ?? COMMA17.
        eco_fisc_ing_percap : ?? COMMA17.
        eco_debt_percap  : ?? BEST5.
        eco_deficit      : ?? BEST7.
        eco_pib_var      : ?? COMMAX6.
        env_gwh_prod     : ?? BEST6.
        env_gwh_prod_renovable : ?? BEST6.
        env_gwh_consum   : ?? BEST6.
        env_kwh_consum_percap : ?? COMMA6.
        env_co2          : ?? COMMA7.
        env_co2_percap   : ?? COMMA4.
        eco_unployement  : ?? COMMA5.
        eco_pib_percap   : ?? BEST5.
        gov_exp_pib      : ?? COMMA6.
        gov_cor_rate     : ?? COMMA18.
        gov_exp_war      : ?? COMMA7.
        gov_exp_war_percap : ?? BEST3.
        gov_exp_san      : ?? COMMA7.
        gov_exp_san_percap : ?? BEST4.
        gov_exp_edu      : ?? COMMA17.
        gov_exp_edu_percap : ?? COMMA17.
        errores          : ?? COMMA20.
        party_AP         : ?? BEST1.
        party_BNG        : ?? BEST1.
        party_CC         : ?? BEST1.
        'party_CC.NC'n   : ?? BEST1.
        party_CCC        : ?? BEST1.
        party_CDS        : ?? BEST1.
        party_CIU        : ?? BEST1.
        party_CS         : ?? BEST1.
        party_CUP        : ?? BEST1.
        party_EA         : ?? BEST1.
        party_EE         : ?? BEST1.
        'party_EH.BILDU'n : ?? BEST1.
        party_ERC        : ?? BEST1.
        party_EV         : ?? BEST1.
        party_FN         : ?? BEST1.
        party_HB         : ?? BEST1.
        party_IU         : ?? BEST1.
        party_JC         : ?? BEST1.
        party_MP         : ?? BEST1.
        party_NS         : ?? BEST1.
        party_PA         : ?? BEST1.
        party_PCE        : ?? BEST1.
        party_PNV        : ?? BEST1.
        party_PODEMOS    : ?? BEST1.
        party_PP         : ?? BEST1.
        party_PRC        : ?? BEST1.
        party_PSOE       : ?? BEST1.
        party_UCD        : ?? BEST1.
        party_UP         : ?? BEST1.
        party_UPYD       : ?? BEST1.
        party_VOX        : ?? BEST1.
        wing_LEFT        : ?? BEST1.
        wing_RIGHT       : ?? BEST1.
        poll_firm_ASEP   : ?? BEST1.
        'poll_firm_CELESTE.TEL'n : ?? BEST1.
        poll_firm_CIS    : ?? BEST1.
        poll_firm_DYM    : ?? BEST1.
        poll_firm_ELECTOPANEL : ?? BEST1.
        poll_firm_GAD3   : ?? BEST1.
        poll_firm_GALLUP : ?? BEST1.
        poll_firm_GESOP  : ?? BEST1.
        'poll_firm_HAMALGAMA_MÃ‰TRICA'n : ?? BEST1.
        poll_firm_IMOP   : ?? BEST1.
        poll_firm_METROSCOPIA : ?? BEST1.
        poll_firm_MYWORD : ?? BEST1.
        poll_firm_NC_REPORT : ?? BEST1.
        poll_firm_NOXA   : ?? BEST1.
        poll_firm_OBRADOIRO_SOCIO : ?? BEST1.
        poll_firm_OPINA  : ?? BEST1.
        poll_firm_SIGMA_DOS : ?? BEST1.
        'poll_firm_SIMPLE_LÃ“GICA'n : ?? BEST1.
        'poll_firm_SOCIOMÃ‰TRICA'n : ?? BEST1.
        poll_firm_TNS_DEMOSCOPIA : ?? BEST1.
        'poll_firm_VOX_PÃšBLICA'n : ?? BEST1.
        lead_party_CS    : ?? BEST1.
        lead_party_PODEMOS : ?? BEST1.
        lead_party_PP    : ?? BEST1.
        lead_party_PSOE  : ?? BEST1.
        lead_party_UCD   : ?? BEST1.
        lead2_party_AP   : ?? BEST1.
        lead2_party_ARM  : ?? BEST1.
        lead2_party_CS   : ?? BEST1.
        lead2_party_EA   : ?? BEST1.
        lead2_party_PODEMOS : ?? BEST1.
        lead2_party_PP   : ?? BEST1.
        lead2_party_PSOE : ?? BEST1.
        lead2_party_UCD  : ?? BEST1.
        lead2_party_UP   : ?? BEST1.
        lead2_party_VOX  : ?? BEST1.
        gov_pre_PP       : ?? BEST1.
        gov_pre_PSOE     : ?? BEST1.
        gov_pre_UCD      : ?? BEST1. ;
DATALINES4;
;;;;
/* -------------------------------------------------------------------
STEPFORWARD CODIGO SAS DEL PROCESO
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.TMP1TempTableForPlots);
/* -------------------------------------------------------------------
   Determina el atributo de tipo del conjunto de datos (si se ha definido uno)
   y lo prepara para su adición a la vista/conjunto de datos, que
   se genera en el paso siguiente.
   ------------------------------------------------------------------- */
DATA _NULL_;
	dsid = OPEN("WORK.TRAIN_SEMMA", "I");
	dstype = ATTRC(DSID, "TYPE");
	IF TRIM(dstype) = " " THEN
		DO;
		CALL SYMPUT("_EG_DSTYPE_", "");
		CALL SYMPUT("_DSTYPE_VARS_", "");
		END;
	ELSE
		DO;
		CALL SYMPUT("_EG_DSTYPE_", "(TYPE=""" || TRIM(dstype) || """)");
		IF VARNUM(dsid, "_NAME_") NE 0 AND VARNUM(dsid, "_TYPE_") NE 0 THEN
			CALL SYMPUT("_DSTYPE_VARS_", "_TYPE_ _NAME_");
		ELSE IF VARNUM(dsid, "_TYPE_") NE 0 THEN
			CALL SYMPUT("_DSTYPE_VARS_", "_TYPE_");
		ELSE IF VARNUM(dsid, "_NAME_") NE 0 THEN
			CALL SYMPUT("_DSTYPE_VARS_", "_NAME_");
		ELSE
			CALL SYMPUT("_DSTYPE_VARS_", "");
		END;
	rc = CLOSE(dsid);
	STOP;
RUN;

/* -------------------------------------------------------------------
   No es necesario ordenar el conjunto de datos WORK.TRAIN_SEMMA.
   ------------------------------------------------------------------- */
DATA WORK.SORTTempTableSorted &_EG_DSTYPE_ / VIEW=WORK.SORTTempTableSorted;
	SET WORK.TRAIN_SEMMA(KEEP=errores year_elec n_days_field days_to_elec porc_surveys_firm n est_surv_vote prom_general_partido prom_general_wing prom_casa_partido prom_casa_wing prom_carrera_partido prom_carrera_wing
	  prom_carrera_casa_partido prom_carrera_casa_wing house_effect_e wing_effect_e urna_0 urna_7 urna_15 urna_60 urna_365 pobl_densidad pobl_fem_porc pobl pobl_kill pobl_kill_percienmil
	  pobl_suicide pobl_suicide_percienmil pobl_life_expectancy pobl_idh pobl_im_rate pobl_em_rate pobl_pobreza_rate eco_smi eco_rate_avg eco_fisc_ing eco_fisc_ing_percap eco_debt_percap eco_deficit
	  eco_pib_var env_gwh_prod env_gwh_prod_renovable env_gwh_consum env_kwh_consum_percap env_co2 env_co2_percap eco_unployement eco_pib_percap gov_exp_pib gov_cor_rate gov_exp_war
	  gov_exp_war_percap gov_exp_san gov_exp_san_percap gov_exp_edu gov_exp_edu_percap party_AP party_BNG party_CC "party_CC.NC"n party_CCC party_CDS party_CIU party_CS party_CUP party_EA party_EE
	  "party_EH.BILDU"n party_ERC party_EV party_FN party_HB party_IU party_JC party_MP party_NS party_PA party_PCE party_PNV party_PODEMOS party_PP party_PRC party_PSOE party_UCD party_UP party_UPYD
	  party_VOX wing_LEFT wing_RIGHT poll_firm_ASEP "poll_firm_CELESTE.TEL"n poll_firm_CIS poll_firm_DYM poll_firm_ELECTOPANEL poll_firm_GAD3 poll_firm_GALLUP poll_firm_GESOP
	  "poll_firm_HAMALGAMA_MÃ‰TRICA"n poll_firm_IMOP poll_firm_METROSCOPIA poll_firm_MYWORD poll_firm_NC_REPORT poll_firm_NOXA poll_firm_OBRADOIRO_SOCIO poll_firm_OPINA poll_firm_SIGMA_DOS "poll_firm_SIMPLE_LÃ“GICA"n
	  "poll_firm_SOCIOMÃ‰TRICA"n poll_firm_TNS_DEMOSCOPIA "poll_firm_VOX_PÃšBLICA"n lead_party_CS lead_party_PODEMOS lead_party_PP lead_party_PSOE lead_party_UCD lead2_party_AP lead2_party_ARM
	  lead2_party_CS lead2_party_EA lead2_party_PODEMOS lead2_party_PP lead2_party_PSOE lead2_party_UCD lead2_party_UP lead2_party_VOX gov_pre_PP gov_pre_PSOE gov_pre_UCD &_DSTYPE_VARS_);
RUN;
TITLE;
TITLE1 "Resultados de la regresión lineal";
FOOTNOTE;
FOOTNOTE1 "Generado por SAS (&_SASSERVERNAME, &SYSSCPL) con fecha %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) a las %TRIM(%QSYSFUNC(TIME(), NLTIME.))";
PROC REG DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	Linear_Regression_Model: MODEL errores = year_elec n_days_field days_to_elec porc_surveys_firm n est_surv_vote prom_general_partido prom_general_wing prom_casa_partido prom_casa_wing prom_carrera_partido prom_carrera_wing prom_carrera_casa_partido prom_carrera_casa_wing house_effect_e wing_effect_e urna_0 urna_7 urna_15 urna_60 urna_365 pobl_densidad pobl_fem_porc pobl pobl_kill pobl_kill_percienmil pobl_suicide pobl_suicide_percienmil pobl_life_expectancy pobl_idh pobl_im_rate pobl_em_rate pobl_pobreza_rate eco_smi eco_rate_avg eco_fisc_ing eco_fisc_ing_percap eco_debt_percap eco_deficit eco_pib_var env_gwh_prod env_gwh_prod_renovable env_gwh_consum env_kwh_consum_percap env_co2 env_co2_percap eco_unployement eco_pib_percap gov_exp_pib gov_cor_rate gov_exp_war gov_exp_war_percap gov_exp_san gov_exp_san_percap gov_exp_edu gov_exp_edu_percap party_AP party_BNG party_CC "party_CC.NC"n party_CCC party_CDS party_CIU party_CS party_CUP party_EA party_EE "party_EH.BILDU"n party_ERC party_EV party_FN party_HB party_IU party_JC party_MP party_NS party_PA party_PCE party_PNV party_PODEMOS party_PP party_PRC party_PSOE party_UCD party_UP party_UPYD party_VOX wing_LEFT wing_RIGHT poll_firm_ASEP "poll_firm_CELESTE.TEL"n poll_firm_CIS poll_firm_DYM poll_firm_ELECTOPANEL poll_firm_GAD3 poll_firm_GALLUP poll_firm_GESOP "poll_firm_HAMALGAMA_MÃ‰TRICA"n poll_firm_IMOP poll_firm_METROSCOPIA poll_firm_MYWORD poll_firm_NC_REPORT poll_firm_NOXA poll_firm_OBRADOIRO_SOCIO poll_firm_OPINA poll_firm_SIGMA_DOS "poll_firm_SIMPLE_LÃ“GICA"n "poll_firm_SOCIOMÃ‰TRICA"n poll_firm_TNS_DEMOSCOPIA "poll_firm_VOX_PÃšBLICA"n lead_party_CS lead_party_PODEMOS lead_party_PP lead_party_PSOE lead_party_UCD lead2_party_AP lead2_party_ARM lead2_party_CS lead2_party_EA lead2_party_PODEMOS lead2_party_PP lead2_party_PSOE lead2_party_UCD lead2_party_UP lead2_party_VOX gov_pre_PP gov_pre_PSOE gov_pre_UCD
		/		SELECTION=FORWARD
		SLE=0.5
		INCLUDE=0
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Fin de código de la tarea
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.TMP1TempTableForPlots);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


/* -------------------------------------------------------------------
STEPBACKWARD CODIGO SAS DEL PROCESO
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.TMP1TempTableForPlots);
/* -------------------------------------------------------------------
   Determina el atributo de tipo del conjunto de datos (si se ha definido uno)
   y lo prepara para su adición a la vista/conjunto de datos, que
   se genera en el paso siguiente.
   ------------------------------------------------------------------- */
DATA _NULL_;
	dsid = OPEN("WORK.TRAIN_SEMMA", "I");
	dstype = ATTRC(DSID, "TYPE");
	IF TRIM(dstype) = " " THEN
		DO;
		CALL SYMPUT("_EG_DSTYPE_", "");
		CALL SYMPUT("_DSTYPE_VARS_", "");
		END;
	ELSE
		DO;
		CALL SYMPUT("_EG_DSTYPE_", "(TYPE=""" || TRIM(dstype) || """)");
		IF VARNUM(dsid, "_NAME_") NE 0 AND VARNUM(dsid, "_TYPE_") NE 0 THEN
			CALL SYMPUT("_DSTYPE_VARS_", "_TYPE_ _NAME_");
		ELSE IF VARNUM(dsid, "_TYPE_") NE 0 THEN
			CALL SYMPUT("_DSTYPE_VARS_", "_TYPE_");
		ELSE IF VARNUM(dsid, "_NAME_") NE 0 THEN
			CALL SYMPUT("_DSTYPE_VARS_", "_NAME_");
		ELSE
			CALL SYMPUT("_DSTYPE_VARS_", "");
		END;
	rc = CLOSE(dsid);
	STOP;
RUN;

/* -------------------------------------------------------------------
   No es necesario ordenar el conjunto de datos WORK.TRAIN_SEMMA.
   ------------------------------------------------------------------- */
DATA WORK.SORTTempTableSorted &_EG_DSTYPE_ / VIEW=WORK.SORTTempTableSorted;
	SET WORK.TRAIN_SEMMA(KEEP=errores year_elec n_days_field days_to_elec porc_surveys_firm n est_surv_vote prom_general_partido prom_general_wing prom_casa_partido prom_casa_wing prom_carrera_partido prom_carrera_wing
	  prom_carrera_casa_partido prom_carrera_casa_wing house_effect_e wing_effect_e urna_0 urna_7 urna_15 urna_60 urna_365 pobl_densidad pobl_fem_porc pobl pobl_kill pobl_kill_percienmil
	  pobl_suicide pobl_suicide_percienmil pobl_life_expectancy pobl_idh pobl_im_rate pobl_em_rate pobl_pobreza_rate eco_smi eco_rate_avg eco_fisc_ing eco_fisc_ing_percap eco_debt_percap eco_deficit
	  eco_pib_var env_gwh_prod env_gwh_prod_renovable env_gwh_consum env_kwh_consum_percap env_co2 env_co2_percap eco_unployement eco_pib_percap gov_exp_pib gov_cor_rate gov_exp_war
	  gov_exp_war_percap gov_exp_san gov_exp_san_percap gov_exp_edu gov_exp_edu_percap party_AP party_BNG party_CC "party_CC.NC"n party_CCC party_CDS party_CIU party_CS party_CUP party_EA party_EE
	  "party_EH.BILDU"n party_ERC party_EV party_FN party_HB party_IU party_JC party_MP party_NS party_PA party_PCE party_PNV party_PODEMOS party_PP party_PRC party_PSOE party_UCD party_UP party_UPYD
	  party_VOX wing_LEFT wing_RIGHT poll_firm_ASEP "poll_firm_CELESTE.TEL"n poll_firm_CIS poll_firm_DYM poll_firm_ELECTOPANEL poll_firm_GAD3 poll_firm_GALLUP poll_firm_GESOP
	  "poll_firm_HAMALGAMA_MÃ‰TRICA"n poll_firm_IMOP poll_firm_METROSCOPIA poll_firm_MYWORD poll_firm_NC_REPORT poll_firm_NOXA poll_firm_OBRADOIRO_SOCIO poll_firm_OPINA poll_firm_SIGMA_DOS "poll_firm_SIMPLE_LÃ“GICA"n
	  "poll_firm_SOCIOMÃ‰TRICA"n poll_firm_TNS_DEMOSCOPIA "poll_firm_VOX_PÃšBLICA"n lead_party_CS lead_party_PODEMOS lead_party_PP lead_party_PSOE lead_party_UCD lead2_party_AP lead2_party_ARM
	  lead2_party_CS lead2_party_EA lead2_party_PODEMOS lead2_party_PP lead2_party_PSOE lead2_party_UCD lead2_party_UP lead2_party_VOX gov_pre_PP gov_pre_PSOE gov_pre_UCD &_DSTYPE_VARS_);
RUN;
TITLE;
TITLE1 "Resultados de la regresión lineal";
FOOTNOTE;
FOOTNOTE1 "Generado por SAS (&_SASSERVERNAME, &SYSSCPL) con fecha %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) a las %TRIM(%QSYSFUNC(TIME(), NLTIME.))";
PROC REG DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	Linear_Regression_Model: MODEL errores = year_elec n_days_field days_to_elec porc_surveys_firm n est_surv_vote prom_general_partido prom_general_wing prom_casa_partido prom_casa_wing prom_carrera_partido prom_carrera_wing prom_carrera_casa_partido prom_carrera_casa_wing house_effect_e wing_effect_e urna_0 urna_7 urna_15 urna_60 urna_365 pobl_densidad pobl_fem_porc pobl pobl_kill pobl_kill_percienmil pobl_suicide pobl_suicide_percienmil pobl_life_expectancy pobl_idh pobl_im_rate pobl_em_rate pobl_pobreza_rate eco_smi eco_rate_avg eco_fisc_ing eco_fisc_ing_percap eco_debt_percap eco_deficit eco_pib_var env_gwh_prod env_gwh_prod_renovable env_gwh_consum env_kwh_consum_percap env_co2 env_co2_percap eco_unployement eco_pib_percap gov_exp_pib gov_cor_rate gov_exp_war gov_exp_war_percap gov_exp_san gov_exp_san_percap gov_exp_edu gov_exp_edu_percap party_AP party_BNG party_CC "party_CC.NC"n party_CCC party_CDS party_CIU party_CS party_CUP party_EA party_EE "party_EH.BILDU"n party_ERC party_EV party_FN party_HB party_IU party_JC party_MP party_NS party_PA party_PCE party_PNV party_PODEMOS party_PP party_PRC party_PSOE party_UCD party_UP party_UPYD party_VOX wing_LEFT wing_RIGHT poll_firm_ASEP "poll_firm_CELESTE.TEL"n poll_firm_CIS poll_firm_DYM poll_firm_ELECTOPANEL poll_firm_GAD3 poll_firm_GALLUP poll_firm_GESOP "poll_firm_HAMALGAMA_MÃ‰TRICA"n poll_firm_IMOP poll_firm_METROSCOPIA poll_firm_MYWORD poll_firm_NC_REPORT poll_firm_NOXA poll_firm_OBRADOIRO_SOCIO poll_firm_OPINA poll_firm_SIGMA_DOS "poll_firm_SIMPLE_LÃ“GICA"n "poll_firm_SOCIOMÃ‰TRICA"n poll_firm_TNS_DEMOSCOPIA "poll_firm_VOX_PÃšBLICA"n lead_party_CS lead_party_PODEMOS lead_party_PP lead_party_PSOE lead_party_UCD lead2_party_AP lead2_party_ARM lead2_party_CS lead2_party_EA lead2_party_PODEMOS lead2_party_PP lead2_party_PSOE lead2_party_UCD lead2_party_UP lead2_party_VOX gov_pre_PP gov_pre_PSOE gov_pre_UCD
		/		SELECTION=BACKWARD
		SLS=0.1
		INCLUDE=0
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Fin de código de la tarea
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.TMP1TempTableForPlots);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;

/* -------------------------------------------------------------------
   Código SAS pàra caracterización de datos
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.FREQCharFrequency,
		WORK.UNIVCharUnivariate,
		WORK.TTAFTempTableAccumFreq,
		WORK.TTAUTempTableAccumUniv,
		WORK.TCONTempTableContents,
		WORK.TPFRTempTableFrequencies2,
		WORK.TPUNTempTableUnivariate2,
		WORK.TSPUTempTableUnivariate3,
		WORK.TFFRTempFormatFreqs);
%MACRO REPORTS; 
%IF &charVarsFlag = 1 %THEN 
  %DO; 
OPTIONS MISSING=' ' PAGENO=1; 

TITLE;
TITLE1 "Resumen de variables categóricas para &LIB..&DSN.";
TITLE2 "Limitado a &CATOBS. Valores distintos más frecuentes por variable";
FOOTNOTE;
FOOTNOTE1 "Generado por SAS (&_SASSERVERNAME, &SYSSCPL) con fecha %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) a las %TRIM(%QSYSFUNC(TIME(), NLTIME.))";
PROC PRINT DATA=WORK.TTAFTempTableAccumFreq LABEL; 
	BY Variable Label; 
	ID Variable Label; 
	VAR Value Count Percent; 
	FORMAT Label; 
	; 
RUN; 

  %END; 
%ELSE 
  %DO; 
OPTIONS MISSING=' ' PAGENO=1; 

TITLE;
TITLE1 "Resumen de variables categóricas para &LIB..&DSN.";
TITLE2 "Limitado a &CATOBS. Valores distintos más frecuentes por variable";
FOOTNOTE; 
DATA _NULL_; 
	FILE PRINT; 
	PUT /// "'NO SE HAN ENCONTRADO VARIABLES ALFANUMÉRICAS EN LOS DATOS DE ENTRADA'"; 
  STOP; 
RUN; 
  %END; 

%IF &numVarsFlag = 1 %THEN 
  %DO; 
/* -------------------------------------------------------------------
   Determine qué formatos de fecha, fecha hora y monetarios
están más asociados con las variables del conjunto de datos
de entrada.  Los formatos se 
utilizarán para mostrar los valores de la variable de fecha,
hora, la fecha hora y monetarios en sus respectivos informes
de sumarización.
   ------------------------------------------------------------------- */
PROC FREQ DATA=WORK.TTAUTempTableAccumUniv NOPRINT; 
	TABLES FORMAT / OUT=WORK.TFFRTempFormatFreqs; 
RUN; 

PROC SORT DATA=WORK.TFFRTempFormatFreqs; 
	BY DESCENDING COUNT; 
RUN; 

%LET _EG_DATE_FMT=DATE; 
%LET _EG_TIME_FMT=TIME; 
%LET _EG_DATETIME_FMT=DATETIME; 
%LET _EG_CURRENCY_FMT=DOLLAR; 
DATA _NULL_; 
	RETAIN dateFound timeFound datetimeFound currencyFound 0; 
	SET WORK.TFFRTempFormatFreqs; 
	IF FORMAT IN ("YYQ", "YYQC", "YYQD", "YYQN", "YYQP", "YYQS", 
			"YYQR", "YYQRC", "YYQRD", "YYQRN", "YYQRP", "YYQRS", 
			"MMYY", "MMYYC", "MMYYD", "MMYYN", "MMYYP", "MMYYS", 
			"YYMM", "YYMMC", "YYMMD", "YYMMN", "YYMMP", "YYMMS", 
			"DDMMYY", "DDMMYYB", "DDMMYYC", "DDMMYYD", "DDMMYYN", "DDMMYYP", "DDMMYYS", 
			"MMDDYY", "MMDDYYB", "MMDDYYC", "MMDDYYD", "MMDDYYN", "MMDDYYP", "MMDDYYS", 
			"YYMMDD", "YYMMDDB", "YYMMDDC", "YYMMDDD", "YYMMDDN", "YYMMDDP", "YYMMDDS", 
			"DATE", "HEBDATE", "HDATE", "MINGUO", "YYMON", "NENGO", "DAY", 
			"DOWNAME", "EURDFDN", "EURDFDWN", "EURDFMN", "EURDFWDX", "EURDFWKX", "MONNAME", 
			"MONTH", "NLDATEMN", "NLDATEW", "NLDATWN", "QTR", "QTRR", "WEEKDATE", "WEEKDATX", 
			"WEEKDAY", "WEEKU", "WEEKV", "WEEKW", "WORDDATE", "WORDDATX", "YEAR", 
			"EURDFDE", "EURDFDD", "EURDFMY", "NLDATE", "MONYY", "JULDAY", "JULIAN") THEN 
	  DO; 
		IF dateFound = 0 THEN 
		  DO; 
			CALL SYMPUT("_EG_DATE_FMT", TRIM(FORMAT)); 
			dateFound = 1; 
		  END; 

		RETURN; 
	  END; 

	IF FORMAT IN ("HHMM", "HOUR", "NLTIMAP", "NLTIME", "TIME", "TIMEAMPM", 
			"MMSS", "MMSSC", "MMSSD", "MMSSN", "MMSSP", "MMSSS", "TOD") THEN 
	  DO; 
		IF timeFound = 0 THEN 
		  DO; 
			CALL SYMPUT("_EG_TIME_FMT", TRIM(FORMAT)); 
			timeFound = 1; 
		  END; 

		RETURN; 
	  END; 

	IF FORMAT IN ("DATETIME", "EURDFDT", "DTYYQC", "DATEAMPM", "DTDATE", 
			"DTYYQC", "NLDATMW", "NLDATM", "NLDATMAP", "NLDATMTM", 
			"NLTIMAP", "DTMONYY", "DTWKDATX", "DTYEAR") THEN 
	  DO; 
		IF datetimeFound = 0 THEN 
		  DO; 
			CALL SYMPUT("_EG_DATETIME_FMT", TRIM(FORMAT)); 
			datetimeFound = 1; 
		  END; 

		RETURN; 
	  END; 

	IF FORMAT IN ("DOLLAR", "DOLLARX", "EURO", "EUROX", "NLMNY", "NLMNYI", "YEN") OR 
			SUBSTR(FORMAT, 1, 5) = "EURFR"  OR  SUBSTR(FORMAT, 1, 5) = "EURTO"   THEN 
	  DO; 
		IF currencyFound = 0 THEN 
		  DO; 
			CALL SYMPUT("_EG_CURRENCY_FMT", TRIM(FORMAT)); 
			currencyFound = 1; 
		  END; 

		RETURN; 
	  END; 

RUN; 

PROC SORT DATA=WORK.TTAUTempTableAccumUniv; 
	BY variable label; 
RUN; 

TITLE;
TITLE1 "Resumen de variables numéricas para &LIB..&DSN.";
FOOTNOTE;
FOOTNOTE1 "Generado por SAS (&_SASSERVERNAME, &SYSSCPL) con fecha %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) a las %TRIM(%QSYSFUNC(TIME(), NLTIME.))";
PROC PRINT DATA=WORK.TTAUTempTableAccumUniv; 
	WHERE format NOT IN ("DOLLAR", "DOLLARX", "EURO", "EUROX", "NLMNY", "NLMNYI", "YEN", 
			"YYQ", "YYQC", "YYQD", "YYQN", "YYQP", "YYQS", 
			"YYQR", "YYQRC", "YYQRD", "YYQRN", "YYQRP", "YYQRS", 
			"MMYY", "MMYYC", "MMYYD", "MMYYN", "MMYYP", "MMYYS", 
			"YYMM", "YYMMC", "YYMMD", "YYMMN", "YYMMP", "YYMMS", 
			"DDMMYY", "DDMMYYB", "DDMMYYC", "DDMMYYD", "DDMMYYN", "DDMMYYP", "DDMMYYS", 
			"MMDDYY", "MMDDYYB", "MMDDYYC", "MMDDYYD", "MMDDYYN", "MMDDYYP", "MMDDYYS", 
			"YYMMDD", "YYMMDDB", "YYMMDDC", "YYMMDDD", "YYMMDDN", "YYMMDDP", "YYMMDDS", 
			"DATE", "HEBDATE", "HDATE", "MINGUO", "YYMON", "NENGO", "DAY", 
			"DOWNAME", "EURDFDN", "EURDFDWN", "EURDFMN", "EURDFWDX", "EURDFWKX", "MONNAME", 
			"MONTH", "NLDATEMN", "NLDATEW", "NLDATWN", "QTR", "QTRR", "WEEKDATE", "WEEKDATX", 
			"WEEKDAY", "WEEKU", "WEEKV", "WEEKW", "WORDDATE", "WORDDATX", "YEAR", 
			"EURDFDE", "EURDFDD", "EURDFMY", "NLDATE", "MONYY", "JULDAY", "JULIAN", 
			"HHMM", "HOUR", "NLTIMAP", "NLTIME", "TIME", "TIMEAMPM", 
			"MMSS", "MMSSC", "MMSSD", "MMSSN", "MMSSP", "MMSSS", "TOD", 
			"DATETIME", "EURDFDT", "DTYYQC", "DATEAMPM", "DTDATE", 
			"DTYYQC", "NLDATMW", "NLDATM", "NLDATMAP", "NLDATMTM", 
			"NLTIMAP", "DTMONYY", "DTWKDATX", "DTYEAR") AND 
			FORMAT NOT LIKE "EURFR%" AND FORMAT NOT LIKE "EURTO%"; 
	BY variable label; 
	ID variable label; 
	VAR n nmiss total min mean median max stdmean; 
	FORMAT Label; 
RUN; 

TITLE;
TITLE1 "Resumen de variables monetarias para &LIB..&DSN.";
PROC PRINT DATA=WORK.TTAUTempTableAccumUniv; 
	WHERE format IN ("DOLLAR", "DOLLARX", "EURO", "EUROX", "NLMNY", "NLMNYI", "YEN") OR 
			FORMAT LIKE "EURFR%" OR FORMAT LIKE "EURTO%"; 
	BY variable label; 
	ID variable label; 
	VAR n nmiss total min mean median max stdmean; 
	FORMAT total mean stdmean min median max &_EG_CURRENCY_FMT.16.2; 
	FORMAT Label; 
RUN; 

TITLE;
TITLE1 "Resumen de variables de fecha para &LIB..&DSN.";
PROC PRINT DATA=WORK.TTAUTempTableAccumUniv; 
	WHERE format IN ("YYQ", "YYQC", "YYQD", "YYQN", "YYQP", "YYQS", 
			"YYQR", "YYQRC", "YYQRD", "YYQRN", "YYQRP", "YYQRS", 
			"MMYY", "MMYYC", "MMYYD", "MMYYN", "MMYYP", "MMYYS", 
			"YYMM", "YYMMC", "YYMMD", "YYMMN", "YYMMP", "YYMMS", 
			"DDMMYY", "DDMMYYB", "DDMMYYC", "DDMMYYD", "DDMMYYN", "DDMMYYP", "DDMMYYS", 
			"MMDDYY", "MMDDYYB", "MMDDYYC", "MMDDYYD", "MMDDYYN", "MMDDYYP", "MMDDYYS", 
			"YYMMDD", "YYMMDDB", "YYMMDDC", "YYMMDDD", "YYMMDDN", "YYMMDDP", "YYMMDDS", 
			"DATE", "HEBDATE", "HDATE", "MINGUO", "YYMON", "NENGO", "DAY", 
			"DOWNAME", "EURDFDN", "EURDFDWN", "EURDFMN", "EURDFWDX", "EURDFWKX", "MONNAME", 
			"MONTH", "NLDATEMN", "NLDATEW", "NLDATWN", "QTR", "QTRR", "WEEKDATE", "WEEKDATX", 
			"WEEKDAY", "WEEKU", "WEEKV", "WEEKW", "WORDDATE", "WORDDATX", "YEAR", 
			"EURDFDE", "EURDFDD", "EURDFMY", "NLDATE", "MONYY", "JULDAY", "JULIAN"); 
	BY variable label; 
	ID variable label; 
	VAR n nmiss min mean median max; 
	FORMAT min mean median max &_EG_DATE_FMT..; 
	FORMAT Label; 
RUN; 

TITLE;
TITLE1 "Resumen de variables de hora para &LIB..&DSN.";
PROC PRINT DATA=WORK.TTAUTempTableAccumUniv; 
	WHERE format IN ("HHMM", "HOUR", "NLTIMAP", "NLTIME", "TIME", "TIMEAMPM", 
			"MMSS", "MMSSC", "MMSSD", "MMSSN", "MMSSP", "MMSSS", "TOD"); 
	BY variable label; 
	ID variable label; 
	VAR n nmiss min mean median max; 
	FORMAT min mean median max &_EG_TIME_FMT..; 
	FORMAT Label; 
RUN; 

TITLE;
TITLE1 "Resumen de variables de fecha y hora para &LIB..&DSN.";
PROC PRINT DATA=WORK.TTAUTempTableAccumUniv; 
	WHERE format IN ("DATETIME", "EURDFDT", "DTYYQC", "DATEAMPM", "DTDATE", 
			"DTYYQC", "NLDATMW", "NLDATM", "NLDATMAP", "NLDATMTM", 
			"NLTIMAP", "DTMONYY", "DTWKDATX", "DTYEAR"); 
	BY variable label; 
	ID variable label; 
	VAR n nmiss min mean median max; 
	FORMAT min mean median max &_EG_DATETIME_FMT..; 
	FORMAT Label; 
RUN; 
  %END; 
%ELSE 
  %DO; 
TITLE;
TITLE1 "Resumen de variables numéricas para &LIB..&DSN.";
FOOTNOTE; 
DATA _NULL_; 
	FILE PRINT; 
	PUT /// "'NO SE HAN ENCONTRADO VARIABLES NUMÉRICAS EN LOS DATOS DE ENTRADA'"; 
  STOP; 
RUN; 
  %END; 
%MEND REPORTS; 

%MACRO CHARTS; 
%IF &charVarsFlag = 1 %THEN 
  %DO; 
PROC SORT 
	DATA=WORK.TTAFTempTableAccumFreq; 
	BY Variable; 
RUN; 

TITLE;
TITLE1 "Cuentas de valor de la variable categórica para &LIB..&DSN.";

PROC SGPLOT DATA=WORK.TTAFTempTableAccumFreq NOAUTOLEGEND; 
	YAXIS FITPOLICY=THIN; 
	HBAR value / 
	STAT=SUM 
	RESPONSE=COUNT 
	GROUP=value 
	; 
	BY Variable; 
	RUN; QUIT; 

  %END; 

%IF &numVarsFlag = 1 %THEN 
  %DO; 
PROC SORT DATA=WORK.TTAUTempTableAccumUniv; 
	BY Variable; 
RUN; 

DATA _NULL_; 
	CALL SYMPUT('numVarCount', PUT(numObs, 12.)); 
	STOP; 
	SET WORK.TTAUTempTableAccumUniv NOBS=numObs; 
RUN; 
    %DO obsNumber = 1 %TO &numVarCount.; 
/* -------------------------------------------------------------------
   Se necesita determinar el formato SAS asociado con
la variable actual de manera que se puedan formatear
de manera apropiada  los valores de las medias y de las
medianas para usarlos en las notas al pie de los diagramas.
   ------------------------------------------------------------------- */
DATA _NULL_; 
	pointer = &obsNumber; 
	SET WORK.TTAUTempTableAccumUniv POINT=pointer; 
	dsid=OPEN("&data", "i"); 
	IF dsid > 0 THEN 
	  DO; 
		varno = VARNUM(dsid, Variable); 
		format = " "; 
		IF varno > 0 THEN 
			format = VARFMT(dsid, varno); 
		IF format NE " " THEN 
			CALL SYMPUT("_EG_VAR_FMT", format); 
		ELSE 
			CALL SYMPUT("_EG_VAR_FMT", "BEST12."); 
		rc = CLOSE(dsid); 
	  END; 
	ELSE 
		CALL SYMPUT("_EG_VAR_FMT", "BEST12."); 
	STOP; 
RUN; 

DATA _NULL_; 
	pointer = &obsNumber; 
	SET WORK.TTAUTempTableAccumUniv POINT=pointer; 
	CALL SYMPUT('var', QUOTE(TRIM(Variable))); 
	CALL SYMPUT('var_n', QUOTE(TRIM(Variable)) || "n"); 
	CALL SYMPUT('mean', TRIM(PUT(mean, &_EG_VAR_FMT))); 
	CALL SYMPUT('median', TRIM(PUT(median, &_EG_VAR_FMT))); 
	STOP; 
RUN; 

TITLE;
TITLE1 "Valores de variables numéricas para &LIB..&DSN.";
FOOTNOTE; 
FOOTNOTE1 'Mean = ' "&mean."; 
FOOTNOTE2 'Median = ' "&median."; 
FOOTNOTE3 ' '; 

PROC SGPLOT DATA=&data. NOAUTOLEGEND; 
	XAXIS FITPOLICY=THIN; 
	VBAR &var_n. / 
	STAT=FREQ 
	GROUP=&var_n. 
	; 
	RUN; QUIT; 

	   %END; 
  %END; 
%MEND CHARTS; 

/* -------------------------------------------------------------------
   Defina las variables del conjunto de datos de salida de manera que
existan cuando PROC APPEND las utilice como conjunto de datos BASE
   ------------------------------------------------------------------- */
DATA WORK.FREQCharFrequency(LABEL="Frecuencia total para WORK.TRAIN_SEMMA");  
	LENGTH DataSet $ 41 Variable $32 Label $ 256 Format $ 31 Value $ 32 Count Percent 8;  
	LABEL Count='Frequency Count' Percent='Percent of Total Frequency';  
	RETAIN DataSet Variable Label Format Value ' ' Count Percent 0;  
	STOP;  
RUN;  

DATA WORK.UNIVCharUnivariate(LABEL="Estadísticos univariables para WORK.TRAIN_SEMMA");  
	LENGTH DataSet $ 41 Variable $32 Label $ 256 Format $ 31 N NMiss Total Min Mean Median Max StdMean 8;  
	RETAIN DataSet Variable Label Format ' ' N NMiss Total Min Mean Median Max StdMean 0;  
	STOP;	 
RUN; 

%global dataset obs; 

%MACRO _EG_CHARACT(data, lib, dsn, catobs);  

/* -------------------------------------------------------------------
   Defina las variables en los conjuntos de datos de recopilación de trabajo
y depúrelos de manera que los estadísticos se puedan registrar para 
el conjunto de datos actual.
   ------------------------------------------------------------------- */
DATA WORK.TTAFTempTableAccumFreq;  
	LENGTH DataSet $ 41 Variable $32 Label $ 256 Format $ 31 Value $ 32 Count Percent 8;  
	LABEL Count='Frequency Count' Percent='Percent of Total Frequency';  
	RETAIN DataSet Variable Label Format Value ' ' Count Percent 0;  
	STOP;  
RUN;  

DATA WORK.TTAUTempTableAccumUniv;  
	LENGTH DataSet $ 41 Variable $32 Label $ 256 Format $ 31 N NMiss Total Min Mean Median Max StdMean 8;  
	RETAIN DataSet Variable Label Format ' ' N NMiss Total Min Mean Median Max StdMean 0;  
	STOP;	 
RUN; 

/* -------------------------------------------------------------------
   Obtener toda la información de la variable para el conjunto de datos de entrada.
   ------------------------------------------------------------------- */
PROC CONTENTS 
	DATA=&data. 
	OUT=WORK.TCONTempTableContents  
	NOPRINT; 
RUN;  

/* -------------------------------------------------------------------
   Obtener el número de variables en el conjunto de datos de entrada.
   ------------------------------------------------------------------- */
DATA _NULL_; 
	CALL SYMPUT('numobs',PUT(numobs, 12.)); 
/* -------------------------------------------------------------------
   No necesita leer las observaciones. Sólo
interesa el número de observaciones.
   ------------------------------------------------------------------- */
	STOP; 
	SET WORK.TCONTempTableContents NOBS=numobs; 
RUN; 

/* -------------------------------------------------------------------
   Cada vez que se ejecuta la macro se tiene que inicializar las marcas
de tipo de macro variable. La utiliza el código de generación de conjuntos de datos
de gráficos, de informes y de salida para determinar si los datos existen para poder procesarlos.
   ------------------------------------------------------------------- */
%LET charVarsFlag = 0; 
%LET numVarsFlag = 0; 
/* -------------------------------------------------------------------
   Buscar las variables en el conjunto de datos de entrada y
dependiendo del tipo (alfanumérica o numérica) recopilar  
los estadísticos relevantes para estos valores.
   ------------------------------------------------------------------- */
%DO i=1 %to &numobs.; 
/* -------------------------------------------------------------------
   Cree macro variables que den información sobre
la variable actual y pasos DATA y PROC
posteriores.
   ------------------------------------------------------------------- */
	DATA _NULL_; 
		POINTER=&i.; 
		SET WORK.TCONTempTableContents point=pointer; 
		CALL SYMPUT('var', QUOTE(name)); 
		CALL SYMPUT('var_n', QUOTE(name) || "n"); 
		CALL SYMPUT('type', PUT(type, 1.)); 
		CALL SYMPUT('label', label); 
		CALL SYMPUT('format', format); 
		STOP; 
	RUN; 

/* -------------------------------------------------------------------
   Procese la variable si es numérica.
   ------------------------------------------------------------------- */
	%IF &type.=1 %THEN %DO; 
/* -------------------------------------------------------------------
   Establezca la marca de la macro variable para que indique que el 
conjunto de datos de entrada contiene al menos una
variable numérica.
   ------------------------------------------------------------------- */
		%LET numVarsFlag = 1; 
/* -------------------------------------------------------------------
   Obtener los estadísticos para la variable numérica.
   ------------------------------------------------------------------- */
		PROC UNIVARIATE DATA=&data. NOPRINT; 
			VAR &var_n.; 
			OUTPUT  
				OUT=WORK.TPUNTempTableUnivariate2  
				N=N  
				NMISS=NMiss  
				MEAN=Mean  
				MIN=Min  
				MAX=Max  
				MEDIAN=Median  
				STDMEAN=StdMean  
				SUM=Total; 
		RUN; 

/* -------------------------------------------------------------------
   Añadir los estadísticos para la variable numérica
al conjunto de datos utilizado para recopilar información
sobre las variables numéricas en el conjunto de datos actual.
   ------------------------------------------------------------------- */
		DATA WORK.TTAUTempTableAccumUniv; 
			SET WORK.TTAUTempTableAccumUniv WORK.TPUNTempTableUnivariate2(IN=intemp); 

			IF intemp = 1 THEN DO; 
				Variable=&var.; 
				Label="%nrbquote(&label.)"; 
				DataSet="&lib..&dsn."; 
				Format="&FORMAT."; 
			END; 
		RUN; 
	%END; 

/* -------------------------------------------------------------------
   Procese la variable si es alfanumérica.
   ------------------------------------------------------------------- */
	%ELSE %DO; 
/* -------------------------------------------------------------------
   Establezca la marca de la macro variable para que indique que
los datos de entrada contienen al menos una
variable alfanumérica
   ------------------------------------------------------------------- */
		%LET charVarsFlag = 1; 
/* -------------------------------------------------------------------
   Obtener los estadísticos de frecuencia para los valores
de la variable alfanumérica.
   ------------------------------------------------------------------- */
		PROC FREQ DATA=&data. NOPRINT; 
			TABLES &var_n./MISSING OUT=WORK.TPFRTempTableFrequencies2; 
		RUN; 

/* -------------------------------------------------------------------
   Añadir los totales de frecuencia del valor para la
variable alfanumérica al conjunto de datos utilizado para
recopilar información sobre todas las 
variables alfanuméricas en el conjunto de datos actual.
   ------------------------------------------------------------------- */
		DATA WORK.TTAFTempTableAccumFreq; 
			DROP InVar; 
			LENGTH Value $ 32; 
			SET WORK.TTAFTempTableAccumFreq 
			    WORK.TPFRTempTableFrequencies2(IN=intemp RENAME=(&var_n.=InVar)); 

			IF intemp = 1THEN DO; 
				Value=InVar; 
				Variable=&var.; 
				Label="%nrbquote(&label.)"; 
				DataSet="&lib..&dsn."; 
				Format="&FORMAT."; 
			END; 
		RUN; 
	%END; 
%END; 

/* -------------------------------------------------------------------
   Los datos alfanuméricos necesitan procesos adicionales.
   ------------------------------------------------------------------- */
%IF &charVarsFlag = 1 %THEN 
  %DO; 
/* -------------------------------------------------------------------
   Ordene la información de la variable alfanumérica acumulada 
por nombre y frecuencia total del valor
   ------------------------------------------------------------------- */
PROC SORT DATA=WORK.TTAFTempTableAccumFreq; 
	WHERE dataset NE ' '; 
	BY variable label descending count; 
RUN; 

/* -------------------------------------------------------------------
   Proporcionar una etiqueta para los valores ausentes y si
el número de valores categóricos reportados
debe limitarse, todas las frecuencias de los valores
categóricos se acumulan en elemento "all others" adicional.
   ------------------------------------------------------------------- */
DATA WORK.TTAFTempTableAccumFreq; 
	DROP i newcount newperc; 
	RETAIN i newcount newperc 0; 
	SET WORK.TTAFTempTableAccumFreq; 
	BY variable; 
	IF value=' ' THEN 
		value='***Ausente***'; 
  %IF %EVAL(&catobs.) NE -1 %THEN 
    %DO; 
	IF FIRST.variable = 1 THEN 
		i=1; 
	ELSE 
		i=i+1; 
	IF i > %EVAL(&catobs.) THEN DO; 
		newcount=newcount+count; 
		newperc=newperc+percent; 
	END; 
	IF i > %EVAL(&catobs.) AND LAST.variable = 0 THEN 
		DELETE; 
	IF LAST.variable & i > %EVAL(&catobs.) THEN DO; 
		value='***Resto de valores***'; 
		count=newcount; 
		percent=newperc; 
		newcount=0; 
		newperc=0; 
	END; 
    %END; 
RUN; 
  %END; 

/* -------------------------------------------------------------------
   Invoca la macro para generar informes de sumarización.
   ------------------------------------------------------------------- */
%REPORTS 

/* -------------------------------------------------------------------
   Invoca la macro para generar gráficos.
   ------------------------------------------------------------------- */
%CHARTS 

/* -------------------------------------------------------------------
   Cree los conjuntos de datos de salida.
   ------------------------------------------------------------------- */
%IF &charVarsFlag = 1 %THEN 
  %DO; 
PROC APPEND BASE=WORK.FREQCharFrequency DATA=WORK.TTAFTempTableAccumFreq FORCE; 
RUN; 
  %END; 

%IF &numVarsFlag = 1 %THEN 
  %DO; 
PROC APPEND BASE=WORK.UNIVCharUnivariate DATA=WORK.TTAUTempTableAccumUniv FORCE; 
RUN; 
  %END; 

%MEND _EG_CHARACT; 

%_EG_CHARACT(WORK.TRAIN_SEMMA, WORK, TRAIN_SEMMA, 30); 
/* -------------------------------------------------------------------
   Fin de código de la tarea
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.TTAFTempTableAccumFreq,
		WORK.TTAUTempTableAccumUniv,
		WORK.TCONTempTableContents,
		WORK.TPFRTempTableFrequencies2,
		WORK.TPUNTempTableUnivariate2,
		WORK.TSPUTempTableUnivariate3,
		WORK.TFFRTempFormatFreqs);
TITLE; FOOTNOTE;


