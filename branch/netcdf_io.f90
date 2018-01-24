module netcdf_io_mod

  use netcdf

  implicit none

  private

  public::setup_netcdf_file
  public::nc_read_var
  public::get_timestep

contains

  subroutine nc_read_time_attr(ncid, first_time, size_time, calendar, unit_time)
    implicit none
    integer, intent(in)            :: ncid
    real   , intent(out)           :: first_time
    integer,intent(out)            :: size_time
    character(*),intent(out)       :: unit_time
    character(*),intent(out)       :: calendar
    integer                        :: timeID
    integer                        :: time_dim_id
    real, allocatable              :: time_array(:)

    call check(nf90_inq_dimid(ncid, "time", time_dim_id),"NO time dimension") !time dimension in netcdf has to be "time"
    call check(nf90_inquire_dimension(ncid, time_dim_id, len=size_time), "Error reading time dimension size")
    call check(nf90_inq_varid(ncid, "time", timeID),"NO time variables") !time variable in netcdf has to be "time"
    call check(nf90_get_att(ncid, timeID, "calendar", calendar), "there is no calendar attribute")
    call check(nf90_get_att(ncid, timeID, "units", unit_time), "there is no unit attribute")
    allocate(time_array(10))
    call check(nf90_get_var(ncid, timeID, time_array, (/1/), (/10/)), &
                            "Error Reading getting time variable" ) !pass varname to check so it can give us more info
    first_time = time_array(1)

  end subroutine

  subroutine get_timestep(ncid, timestep)
    ! get time step in netcdf at which variable is extracted
    use PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
    implicit none
    integer,intent(in)       :: ncid
    integer,intent(out)      :: timestep
    integer                  :: size_time
    character(len=30)        :: unit_time
    character(len=30)        :: calendar
    real                     :: first_time
    integer                  :: ref_yr, ref_mon, ref_day, ref_hr, ref_min
    integer                  :: ref_jday, first_jday, now_jday
    real                     :: ref_sec
    ! Functions
    integer, external        :: compute_julday

    call nc_read_time_attr(ncid, first_time, size_time, calendar, unit_time)
    call extractTime(unit_time, ref_yr, ref_mon, ref_day, ref_hr, ref_min, ref_sec)
    ref_jday =  compute_julday(ref_yr, ref_mon, ref_day)
    first_jday = ref_jday + int(first_time)
    now_jday = compute_julday(Nowyear, Nowmonth, Nowday)
    timestep = now_jday - first_jday + 1

  end subroutine

  subroutine nc_read_var(ncid, varid, data_array, timestep)
    implicit none
    integer, intent(in)     :: ncid
    integer, intent(in)     :: varid
    real   , intent(inout)  :: data_array(:)
    integer, intent(in)     :: timestep

    integer, parameter      :: ndims = 2
    integer                 :: dimsize(ndims)
    integer                 :: dimstart(ndims)

    dimstart(1) = 1
    dimstart(2) = timestep
    dimsize(1) = size(data_array) !all the hrus
    dimsize(2) = 1

    call check(nf90_get_var(ncid, varid, data_array,  &
                            dimstart,                 &
                            dimsize,                  &
                            [ 1, 1 ] ),               &
                            "Error Reading Getting variable" ) !pass varname to check so it can give us more info

  end subroutine nc_read_var

  ! yes, this is a little silly, but it works
  subroutine setup_netcdf_file(netcdf_filename, NCfile_unit, &
                               varname1, var_unit1, &
                               varname2, var_unit2, &
                               varname3, var_unit3, &
                               varname4, var_unit4)
      implicit none
      character(len=*), intent(in) :: netcdf_filename, varname1, varname2, varname3, varname4
      integer, intent(inout) :: NCfile_unit, var_unit1, var_unit2, var_unit3, var_unit4

      if (NCfile_unit < 0) then
          call check(nf90_open(netcdf_filename, NF90_NOWRITE, NCfile_unit), trim(netcdf_filename))
      endif

      call check(nf90_inq_varid(NCfile_unit, varname1, var_unit1),trim(netcdf_filename)//":"//trim(varname1))
      call check(nf90_inq_varid(NCfile_unit, varname2, var_unit2),trim(netcdf_filename)//":"//trim(varname2))
      call check(nf90_inq_varid(NCfile_unit, varname3, var_unit3),trim(netcdf_filename)//":"//trim(varname3))
      call check(nf90_inq_varid(NCfile_unit, varname4, var_unit4),trim(netcdf_filename)//":"//trim(varname4))

  end subroutine setup_netcdf_file

  subroutine check(error, error_message)
      implicit none
      integer, intent(in) :: error
      character(len=*), intent(in), optional :: error_message

      if (error /= NF90_NOERR) then
          print*, "NetCDF ERROR :", error
          if (present(error_message)) then
              print*, error_message
          endif
          stop "NetCDF Error"
      endif

  end subroutine check

  ! private subroutine extractTime: extract year/month/day/hour/minute/second from units string
  subroutine extractTime(refdate,iyyy,im,id,ih,imin,dsec)
    implicit none
    ! dummy variables
    character(*), intent(in)    :: refdate             ! units string (time since...)
    integer, intent(out)        :: iyyy,im,id,ih,imin  ! time (year/month/day/hour/minute)
    real, intent(out)           :: dsec                ! seconds
    ! local variables
    integer                     :: err                 ! error code
    character(len=20)           :: message             ! error message
    integer                     :: n                   ! length of the string
    integer                     :: istart,iend         ! position in string

    ! iniitalize error control
    err=0; message="extractTime/"

    ! get the length of the string
    n = len_trim(refdate)
    ! move to a position in string past the time units (seconds since , days since , hours since )
    istart = index(refdate,'since')  ! get the index at the beginning of the word "since"
    if (istart>0) then ! if the word "since" exists
      iend   = index(refdate(istart:n)," ")
      istart = istart+iend
    else
      istart=1
    end if

    ! get the year
    call extract(refdate(istart:n),"-",iend,iyyy,err,message); if (err/=0) return
    if(iyyy < 1900)then; err=20; message=trim(message)//'year < 1900'; return; end if
    if(iyyy > 2100)then; err=20; message=trim(message)//'year > 2100'; return; end if
    ! get the month
    istart=istart+iend
    call extract(refdate(istart:n),"-",iend,im,err,message);   if (err/=0) return
    if(im <  1)then; err=20; message=trim(message)//'month < 1'; return; end if
    if(im > 12)then; err=20; message=trim(message)//'month > 12'; return; end if
    ! get the day
    istart=istart+iend
    call extract(refdate(istart:n)," ",iend,id,err,message);   if (err/=0) return
    if(id <  1)then; err=20; message=trim(message)//'day < 1'; return; end if
    if(id > 31)then; err=20; message=trim(message)//'day > 31'; return; end if
    ! check if we are at the end of the string
    if (istart+(iend-2)==n) then
      ih=0; imin=0; dsec=0.0; return
    end if

    ! get the hour (":" at end of hour)
    istart = istart+iend
    if(istart > len_trim(refdate))then; err=20; message=trim(message)//'string does not include hours'; return; end if
    call extract(refdate(istart:n),":",iend,ih,err,message);   if (err/=0) return
    if(ih <  0)then; err=20; message=trim(message)//'hour < 0'; return; end if
    if(ih > 24)then; err=20; message=trim(message)//'hour > 24'; return; end if
    ! get the minute (":" at end of minute)
    istart = istart+iend
    if(istart > len_trim(refdate))then; err=20; message=trim(message)//'string does not include minutes'; return; end if
    call extract(refdate(istart:n),":",iend,imin,err,message); if (err/=0) return
    if(imin <  0)then; err=20; message=trim(message)//'minute < 0'; return; end if
    if(imin > 60)then; err=20; message=trim(message)//'minute > 60'; return; end if

    ! get the second
    istart = istart+iend
    if(istart > len_trim(refdate)) return
    iend = index(refdate(istart:n)," ")
    read(refdate(istart:n),*) dsec
    !write(*,'(a,i4,1x,4(i2,1x))') 'refdate: iyyy, im, id, ih, imin = ', iyyy, im, id, ih, imin

    contains

      ! internal subroutine extract: extract substring
      subroutine extract(substring,cdelim,iend,itemp,err,message)
        implicit none
        ! dummy variables
        character(*), intent(in)     :: substring  ! sub-string to process
        character(len=1), intent(in) :: cdelim     ! string delimiter
        integer, intent(out)         :: iend       ! index at the end of desired string
        integer, intent(out)         :: itemp      ! output date
        integer, intent(out)         :: err        ! error code
        character(*),intent(out)     :: message    ! error message
        ! initialize error code and message
        err=0; message="extract/"
        ! identify end-point of string
        iend = index(substring,cdelim)
        ! if sub-string does not exist, assume end is at end of string
        if (iend==0) iend=len_trim(substring)+1
        ! convert string to integer
        read(substring(1:iend-1),*,iostat=err) itemp
        ! read error
        if (err/=0) then
          err=20; message=trim(message)//"unexpected characters [string='"//trim(substring)//"']"; return
        end if
      end subroutine extract

  end subroutine extractTime

end module netcdf_io_mod
