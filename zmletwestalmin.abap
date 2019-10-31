REPORT zmletestalmin.

PARAMETERS : p_table LIKE rsrd1-tbma_val OBLIGATORY
           , p_search TYPE string VISIBLE LENGTH 60
           , p_rfelds TYPE char255
           , p_sort TYPE char255
           , p_total TYPE char255.
DATA(z_alv) =  NEW z_alv_ida( ).

DATA(lo_alv_display) = z_alv->create_object( CONV #( p_table ) ).


START-OF-SELECTION.
*------------------

  z_alv->alv_ida_standard(
    EXPORTING
      field_names_remove = p_rfelds
      table_name         = CONV #( p_table )
      field_names_sort   = p_sort
      field_names_total  = p_total
      search_string      = p_search
    IMPORTING
      balret_tab         = DATA(lit_balret_overall)
    CHANGING
      lo_alv_display     = lo_alv_display
      z_alv              = z_alv ).

END-OF-SELECTION.
*----------------


  lo_alv_display->fullscreen( )->display( ).
  
  *************Start of selction texts**************
  P_BLOCK	. (= Dict ref )
P_DSPRET	Display return infotmation
P_LHNDLE	.
P_RFELDS	Fields for removal
P_SEARCH	Test search string
P_SORT	Fields for sorting
P_SRTF2	.
P_TABLE	Table name
P_TOTAL	Fields for totalling
________	______________________________
  
