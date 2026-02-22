! file_helper module - file and directory operations
!
! MIT License
!
! Copyright (c) 2026 Patrizia Favaron
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
module file_helper

    use datetime

    implicit none
    
    private
    
    ! Public interface
    public  :: data_files
    public  :: get_time_stamp
    public  :: basename
    
    ! Data types
    
    type data_files
        character(len=256), dimension(:), allocatable   :: svFileName
        integer, dimension(:), allocatable              :: ivTimeStamp
        integer, dimension(:), allocatable              :: ivLength
    contains
        procedure   :: get
    end type data_files
    
contains

    function get(this, sPathName) result(iRetCode)
    
        ! Routine arguments
        class(data_files), intent(out)  :: this
        character(len=*), intent(in)    :: sPathName
        integer                         :: iRetCode
        
        ! Locals
        integer     :: iErrCode
        integer     :: iFromTime
        integer     :: iToTime
        integer     :: iHour
        integer     :: iNumHours
        type(time)  :: tCurrent
        logical     :: lIsFile
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Get initial and final time instants in month, and number of hours expected
        iErrCode = get_path_date_range(sPathName, iFromTime, iToTime, iNumHours)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        
        ! Use the number of hours to reserve space
        if(allocated(this % svFileName))  deallocate(this % svFileName)
        if(allocated(this % ivLength))    deallocate(this % ivLength)
        if(allocated(this % ivTimeStamp)) deallocate(this % ivTimeStamp)
        allocate(this % svFileName(iNumHours))
        allocate(this % ivLength(iNumHours))
        allocate(this % ivTimeStamp(iNumHours))
        
        ! Fill file names and their time stamps
        this % ivTimeStamp = [(iFromTime + 3600*(iHour-1), iHour=1, iNumHours)]
        do iHour = 1, iNumHours
            tCurrent = FromEpoch(iFromTime + 3600*(iHour-1))
            write(this % svFileName(iHour), "(a,'/',i4.4,2i2.2,'.',i2.2,'R')") &
                trim(sPathName), &
                tCurrent % iYear, tCurrent % iMonth, tCurrent % iDay, &
                tCurrent % iHour
        end do
        
        ! Lookup the file names, and find which of them do really
        ! exist in directory
        do iHour = 1, iNumHours
            inquire(file=this % svFileName(iHour), exist=lIsFile)
            if(lIsFile) then
                inquire(file=this % svFileName(iHour), size=this % ivLength(iHour))
            else
                this % ivLength(iHour) = -1
            end if
        end do
        
    end function get
    
    
    function get_time_stamp(iTimeStamp) result(sTimeStamp)
    
        ! Routine arguments
        integer, intent(in) :: iTimeStamp
        character(len=19)   :: sTimeStamp
        
        ! Locals
        ! --none--
        
        ! Get the information desired
        sTimeStamp = ToString(FromEpoch(iTimeStamp))
        
    end function get_time_stamp
    
    
    function basename(sFileName) result(sBaseName)
    
        ! Routine arguments
        character(len=*), intent(in)    :: sFileName
        character(len=256)              :: sBaseName
        
        ! Locals
        integer :: iPos
        
        ! Get the information desired
        iPos = index(sFileName, '/', back=.true.)
        sBaseName = sFileName(iPos+1:)
        
    end function basename
    
    ! *********************
    ! * Internal routines *
    ! *********************

    ! This function, given the full path name of a "Metek monthly" directory
    ! yields the two bounding dates and the number of hours expected in the
    ! bounding range
    function get_path_date_range(sMetekPath, iTimeFrom, iTimeTo, iNumHours) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)    :: sMetekPath
        integer, intent(out)            :: iTimeFrom
        integer, intent(out)            :: iTimeTo
        integer, intent(out)            :: iNumHours
        integer                         :: iRetCode
        
        ! Locals
        integer     :: iErrCode
        integer     :: iPos
        integer     :: iYear
        integer     :: iMonth
        type(time)  :: tBegin
        type(time)  :: tEnd
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check input arguments
        if(len_trim(sMetekPath) < 6) then
            iRetCode = 1
            return
        end if
        iPos = index(sMetekPath, '/', back = .true.)
        read(sMetekPath(iPos+1:), "(i4,i2)", iostat=iErrCode) iYear, iMonth
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        if(len_trim(sMetekPath(iPos+1:)) /= 6) then
            iRetCode = 3
            return
        end if
        if(iMonth < 1 .or. iMonth > 12) then
            iRetCode = 4
            return
        end if
        if(iYear < 1995) then
            iRetCode = 5
            return
        end if
        
        ! Establish initial and final times
        tBegin = time(int(iYear, kind=2), int(iMonth, kind=1), 1_1, 0_1, 0_1, 0_1)
        iMonth = iMonth + 1
        if(iMonth > 12) then
            iMonth = 1
            iYear = iYear + 1
        end if
        tEnd = time(int(iYear, kind=2), int(iMonth, kind=1), 1_1, 0_1, 0_1, 0_1)
        iTimeFrom = ToEpoch(tBegin)
        iTimeTo = ToEpoch(tEnd) - 3600
        iNumHours = (iTimeTo - iTimeFrom) / 3600 + 1
        
    end function get_path_date_range

end module file_helper
