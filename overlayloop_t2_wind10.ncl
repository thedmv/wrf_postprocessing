 filename = "WRFDATA"
  a = addfile(filename,"r")

;---Read several WRF variables at first time step
  it  = 0
  slp = wrf_user_getvar(a,"slp",it)   ; sea level pressure
  wrf_smooth_2d( slp, 3 )             ; smooth slp
  tk  = wrf_user_getvar(a,"T2",it)    ; 2m Temp in Kelvin
  uv  = wrf_user_getvar(a,"uvmet10",it) ; UVMET winds at 10m

;---Now get the lowest (bottommost) level
  nl  = 0
;  tc2 = tk(nl,:,:)
  u10 = uv(0,:,:)
  v10 = uv(1,:,:)

  tf2 = tk - 273.15                     ; Keep in Celsius
  u10 = u10*1.94386                    ; Convert wind into knots
  v10 = v10*1.94386

;---Change the metadata
  tf2@description = "Surface Temperature"
  tf2@units       = "degC"
  u10@units       = "knots"
  v10@units       = "knots"

;---Attach the shapefile
  nyc_shp_name           = "/mnt/lfs3/projects/wrfruc/dmv.ccny/POSTPROC/dtl_counties_northeast.shp"
  lnres                  = True
  lnres@gsLineColor      = "black"
  lnres@gsLineThicknessF = 0.5

  wks = gsn_open_wks("png","IMGTITLE_t2winds10_WRFDATA")

;---Set common resources
  res                = True
  res@MainTitle      = "WRFDATA" 
;---Temperature filled contour plot
  tf_res                   = res                         
  tf_res@cnFillOn          = True  
  tf_res@ContourParameters = (/16., 48., 2./)
  contour_tf = wrf_contour(a,wks,tf2,tf_res)

;---SLP line contour plot
  slp_res                                = res         
  slp_res@cnLineColor                    = "blue"
  slp_res@cnHighLabelsOn                 = True
  slp_res@cnLowLabelsOn                  = True
  slp_res@ContourParameters              = (/ 900, 1100, 4 /)
  slp_res@cnLineLabelBackgroundColor     = -1
  slp_res@gsnContourLineThicknessesScale = 2.0

  contour_psl = wrf_contour(a,wks,slp,slp_res)

;---Wind vector plot
  vec_res            = res         
  vec_res@FieldTitle = "Wind"       ; overwrite Field Title
  vec_res@NumVectors = 30           ; density of wind barbs

  vector = wrf_vector(a,wks,u10,v10,vec_res)
  
;---Overlay options
  pltres              = True  ; Basic overlay plot options
  pltres@PanelPlot    = True  ; Tells wrf_map_overlays not to remove overlays
  pltres@CommonTitle  = True  
  pltres@PlotTitle    = str_concat((/"Urb. Phys. = ", tostring(a@SF_URBAN_PHYSICS), "; PBL = ", tostring(a@BL_PBL_PHYSICS)/))

  map_res             = True  ; set map options
  map_res@mpOutlineOn = False ; Turn off map outlines
  map_res@mpFillOn    = False ; Turn off map fill

;---Overlay plots on map and draw
  ov = wrf_map_overlays(a,wks,(/contour_tf,contour_psl,vector/),pltres,map_res)
  nyc_id = gsn_add_shapefile_polylines(wks, ov, nyc_shp_name,lnres)
  draw(ov)
  frame(wks)
