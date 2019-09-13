;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get UVMET for a subset of HRRR model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This script is for extracting a subset of 
; U, V, XLAT, XLONG so that we can use
; the uvmet function.
; Input:  tmpMerged.nc -- Contains U, V, XLAT, XLONG, XTIME
;                         from the wrfout* files using ncks
; Output: uvmet.d01.nc -- Contains the rotated and unstaggered
;                         U, V data, with the corresponding
;                         XLAT, XLONG, and XTIME.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---Get the file name for the merged HRRR domain U and V files (tmpuv*.nc)
print("Status: Load Merged file of U,V...")
wrff = addfile("tmpMerged.nc", "r")
XLAT = wrff->XLAT(0,:,:)        ; need 2D for region_ind
XLONG = wrff->XLONG(0,:,:)      ; need 2D for region_ind

;---Values of bounding box of interest
print("Status: Assign bounding box parameters...")
latmin = 40.2
latmax = 41.2
lonmin = -74.7
lonmax = -73.4

;---Store to local variables for better code readability
print("Getting ij for bbox...")
ij = region_ind(XLAT,XLONG, latmin,latmax, lonmin,lonmax)
ilat1 = ij(0)
ilat2 = ij(1)
ilon1 = ij(2)
ilon2 = ij(3)

;---Store subset in nc file
print("Status: Creating nc subset...")
wrff_sub          = addfile("tmpMerged_sub.nc", "c")
wrff_sub->U       = wrff->U(:,:, ilat1:ilat2, ilon1:ilon2)
wrff_sub->V       = wrff->V(:,:, ilat1:ilat2, ilon1:ilon2)
wrff_sub->XLAT    = wrff->XLAT(:, ilat1:ilat2, ilon1:ilon2)
wrff_sub->XLONG   = wrff->XLONG(:, ilat1:ilat2, ilon1:ilon2)
wrff_sub->XTIME   = wrff->XTIME
fileattdef(wrff_sub, wrff)    ; copy global attributes
delete(wrff)                  ; close wrff
delete(wrff_sub)              ; close wrff_sub, also writes the global attributes to file

;---Get U,V Met variables
print("Status: Calculating uvmet...")
wrff_sub        = addfile("tmpMerged_sub.nc", "r")
uvmet           = wrf_user_getvar(wrff_sub, "uvmet", -1)
outfile         = addfile("uvmet.d01.nc", "c")
outfile->uvmet  = uvmet
outfile->XLAT   = wrff_sub->XLAT
outfile->XLONG  = wrff_sub->XLONG
outfile->XTIME  = wrff_sub->XTIME
fileattdef(outfile, wrff_sub)
delete(wrff_sub)
delete(outfile) 
