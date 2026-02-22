! mfc_v2 module - here the important things happen
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
module mfc_v2

    use datetime

    implicit none
    
    private
    
    ! Public interface
    public  :: sonic
    
    ! Data types
    type sonic
        real(8), dimension(:), allocatable  :: rvTimeStamp
        real(4), dimension(:), allocatable  :: rvU
        real(4), dimension(:), allocatable  :: rvV
        real(4), dimension(:), allocatable  :: rvW
        real(4), dimension(:), allocatable  :: rvT
        integer, dimension(:), allocatable  :: ivQuality
    contains
        procedure   :: get
        procedure   :: describe
    end type sonic
    
contains

    function get(this, sFileName, iFileLength, iNumValid, iNumInvalid, iNumSuspect) result(iRetCode)
    
        ! Routine arguments
        class(sonic), intent(out)       :: this
        character(len=*), intent(in)    :: sFileName
        integer, intent(in)             :: iFileLength
        integer, intent(out)            :: iNumValid
        integer, intent(out)            :: iNumInvalid
        integer, intent(out)            :: iNumSuspect
        integer                         :: iRetCode
        
        ! Constants
        integer, parameter  :: MAX_CLASSES = 4001
        
        ! Locals
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i, j, k
        integer                                 :: iU, iV, iW, iT
        integer                                 :: iLen
        type(Time)                              :: tDateTime
        integer                                 :: iHourStamp
        integer, dimension(0:MAX_CLASSES)       :: ivNumInClass
        integer                                 :: n, m
        integer(2), dimension(:), allocatable   :: ivSecond
        integer(2), dimension(:), allocatable   :: ivU
        integer(2), dimension(:), allocatable   :: ivV
        integer(2), dimension(:), allocatable   :: ivW
        integer(2), dimension(:), allocatable   :: ivT
        integer, dimension(:), allocatable      :: ivClass
        logical, dimension(:), allocatable      :: lvValid
        integer(2), dimension(:), allocatable   :: ivReSecond
        integer(2), dimension(:), allocatable   :: ivReU
        integer(2), dimension(:), allocatable   :: ivReV
        integer(2), dimension(:), allocatable   :: ivReW
        integer(2), dimension(:), allocatable   :: ivReT
        integer, dimension(:), allocatable      :: ivReQuality
        integer                                 :: iNumRepeated
        integer                                 :: iFirstRep
        integer                                 :: iPre
        integer                                 :: iDelta
        integer                                 :: iMinSecond
        integer                                 :: iNumMinSeconds
        integer                                 :: iMaxSecond
        integer                                 :: iNumMaxSeconds
        integer                                 :: iFileSize
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Define hour stamp (will use later, when pushing data to the
        ! circular buffer)
        iLen = len_trim(sFileName)
        read(sFileName(iLen-11:iLen), "(i4,2i2,1x,i2)") &
            tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
            tDateTime % iHour
        tDateTime % iMinute = 0_1
        tDateTime % iSecond = 0_1
        iHourStamp = toEpoch(tDateTime)
        
        ! Try accessing the file in read mode, with stream (binary) access
        open(newunit=iLUN, file=sFileName, action="read", access="stream", status="old", iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        
        ! Read data
        n = iFileLength / 10
        ivNumInClass = 0
        iU = huge(iU)
        iV = huge(iV)
        iW = huge(iW)
        iT = huge(iT)
        allocate(ivSecond(n), ivU(n), ivV(n), ivW(n), ivT(n), ivClass(n), lvValid(n))
        allocate(ivReSecond(n), ivReU(n), ivReV(n), ivReW(n), ivReT(n), ivReQuality(n))
        do i = 1, n
            read(iLUN) ivSecond(i), ivV(i), ivU(i), ivW(i), ivT(i)
            if(ivSecond(i) < 5000) then
            
                ! Sonic quadruple: save it
                if(ivU(i) /= iU .or. ivV(i) /= iV .or. ivW(i) /= iW .or. ivT(i) /= iT) then
                    lvValid(i) = .true.
                    ivClass(i) = 0
                    ivNumInClass(0) = ivNumInClass(0) + 1
                else
                    lvValid(i) = .false.
                    ivClass(i) = ivClass(i-1) + 1
                    if(ivClass(i) <= 100) then
                        ivNumInClass(ivClass(i)) = ivNumInClass(ivClass(i)) + 1
                    else
                        ivNumInClass(MAX_CLASSES) = ivNumInClass(MAX_CLASSES) + 1
                    end if
                end if
                iU = ivU(i)
                iV = ivV(i)
                iW = ivW(i)
                iT = ivT(i)
                
            end if
        end do
        close(iLUN)
        
        ! Second phase: Make errors explicit, and convert to standard units
        i = n
        j = 0
        iNumValid = 0
        iNumSuspect = 0
        iNumInvalid = 0
        do while(i >= 1)
        
            if(ivClass(i) == 0) then
                
                ! Class 0 data at 'i'. valid
                j = j + 1
                ivReSecond(j)  = ivSecond(i)
                ivReU(j)       = ivU(i)
                ivRev(j)       = ivV(i)
                ivReW(j)       = ivW(i)
                ivReT(j)       = ivT(i)
                ivReQuality(j) = 0  ! Valid
                iNumValid      = iNumValid + 1
                i = i - 1
                
            else
                
                ! Class > 0: sequence of invalids detected
                iNumRepeated = ivClass(i)
                iPre      = i - iNumRepeated
                iFirstRep = iPre + 1
                if(mod(iNumRepeated, 2) == 1) then
                    ! Odd: one for a suspect, then the couples for error
                    
                    ! If one or more couples, count each of them as one error
                    if(iNumRepeated > 1) then
                        k = i - 1
                        do while(k >= iFirstRep)
                            j              = j + 1
                            ivReSecond(j)  = ivSecond(k)
                            ivReU(j)       = -9999
                            ivReV(j)       = -9999
                            ivReW(j)       = -9999
                            ivReT(j)       = -9999
                            ivReQuality(j) = 2  ! Certainly invalid
                            iNumInvalid    = iNumInvalid + 1
                            k = k - 2
                        end do
                    end if
                    
                    ! And last, the suspect data
                    j              = j + 1
                    ivReSecond(j)  = ivSecond(iPre)
                    ivReU(j)       = ivU(iPre)
                    ivRev(j)       = ivV(iPre)
                    ivReW(j)       = ivW(iPre)
                    ivReT(j)       = ivT(iPre)
                    ivReQuality(j) = 1      ! Suspect
                    iDelta         = 1
                    iNumSuspect    = iNumSuspect + 1
                    
                else ! Even: only error couples
                
                    k = i
                    do while(k >= iFirstRep)
                        j = j + 1
                        ivReSecond(j)  = ivSecond(k)
                        ivReU(j)       = -9999
                        ivReV(j)       = -9999
                        ivReW(j)       = -9999
                        ivReT(j)       = -9999
                        ivReQuality(j) = 2  ! Certainly invalid
                        iNumInvalid    = iNumInvalid + 1
                        k = k - 2
                    end do
                    iDelta = 0
                    
                end if
                
                ! Next step
                i = iPre - iDelta
                
            end if
            
        end do
        
        ! Reconstructed file length
        iFileSize = j
        
        ! Assign high-resolution time stamp
        ! -1- Locate initial and final seconds
        m = iFileSize
        if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
        if(allocated(this % rvU))         deallocate(this % rvU)
        if(allocated(this % rvV))         deallocate(this % rvV)
        if(allocated(this % rvW))         deallocate(this % rvW)
        if(allocated(this % rvT))         deallocate(this % rvT)
        if(allocated(this % ivQuality))   deallocate(this % ivQuality)
        allocate(this % rvTimeStamp(m))
        allocate(this % rvU(m))
        allocate(this % rvV(m))
        allocate(this % rvW(m))
        allocate(this % rvT(m))
        allocate(this % ivQuality(m))
        iMinSecond = minval(ivReSecond(1:m))
        iNumMinSeconds = count(ivReSecond(1:m) == iMinSecond)
        this % rvTimeStamp(1) = iMinSecond + 0.1d0*(9-min(9,iNumMinSeconds-1))
        iMaxSecond = maxval(ivReSecond(1:m))
        iNumMaxSeconds = count(ivReSecond(1:m) == iMaxSecond)
        this % rvTimeStamp(m) = iMaxSecond + 0.1d0*(min(9,iNumMinSeconds-1))
        ! -1- Compute the high resolution time stamps
        do i = 2, m-1
            this % rvTimeStamp(i) = this % rvTimeStamp(1) + (i-1)*(this % rvTimeStamp(m) - this % rvTimeStamp(1))/real(m-1,kind=8)
        end do
        
        ! Convert sonic quadruples to SI units, and meanwhile revert back the
        ! data set to ensure it is well-ordered
        do i = 1, m
            j = m - i + 1
            if(ivReQuality(j) <= 1) then
                this % rvU(i) = ivReU(j) / 100.0
                this % rvV(i) = ivReV(j) / 100.0
                this % rvW(i) = ivReW(j) / 100.0
                this % rvT(i) = ivReT(j) / 100.0
            else
                this % rvU(i) = -9999.9
                this % rvV(i) = -9999.9
                this % rvW(i) = -9999.9
                this % rvT(i) = -9999.9
            end if
            this % ivQuality(i) = ivReQuality(j)
        end do
        
        ! Leave
        deallocate(ivReSecond, ivReU, ivReV, ivReW, ivReT, ivReQuality)
        deallocate(ivSecond, ivU, ivV, ivW, ivT, ivClass, lvValid)
        
    end function get
    
    
    function describe( &
        this, &
        rVel, &                 ! Vector wind speed
        rScalarVel, &           ! Scalar wind speed
        rFractionLowSpeed, &    ! Fraction of wind speeds <= 0.5 m/s
        rDir, &                 ! Vector direction (° from N)
        rW, &                   ! Vertical wind speed (m/s)
        rTKE &                  ! Turbulent Kinetic Energy (m2/s2)
    ) result(iRetCode)
    
        ! Routine arguments
        class(sonic), intent(in)    :: this
        real, intent(out)           :: rVel
        real, intent(out)           :: rScalarVel
        real, intent(out)           :: rFractionLowSpeed
        real, intent(out)           :: rDir
        real, intent(out)           :: rW
        real, intent(out)           :: rTKE
        integer                     :: iRetCode
        
        ! Locals
        integer                             :: n
        real(4), dimension(:), allocatable  :: rvVel
        real(4)                             :: rU
        real(4)                             :: rV
        real(4)                             :: rUU
        real(4)                             :: rVV
        real(4)                             :: rWW
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Get individual velocities
        allocate(rvVel(size(this % rvU)))
        n = count(this % rvU > -9999.0)
        if(n <= 0) then
            iRetCode          = 1
            rVel              = -9999.9
            rScalarVel        = -9999.9
            rFractionLowSpeed = -9999.9
            rDir              = -9999.9
            rW                = -9999.9
            rTKE              = -9999.9
        end if
        where(this % rvU > -9999.0)
            rvVel = sqrt(this % rvU**2 + this % rvV**2)
        elsewhere
            rvVel = -9999.9
        end where
        
        ! Velocity components
        rU = sum(this % rvU, mask = this % rvU > -9999.0) / n
        rV = sum(this % rvV, mask = this % rvV > -9999.0) / n
        rW = sum(this % rvW, mask = this % rvW > -9999.0) / n
        
        ! Calculate vector and scalar velocities
        rVel = sqrt(rU**2 + rV**2)
        rScalarVel = sum(rvVel, mask = rvVel > -9999.0) / n
        
        ! Compute the fraction of speeds below 0.5 m/s
        rFractionLowSpeed = 1.0 - count(rvVel > 0.5) / n
        
        ! Compute vector wind direction according to the provenance convention
        rDir = 180./3.1415926*atan2(-rU, -rV)
        if(rDir < 0.) rDir = rDir + 360.0
        
        ! Turbulent kinetic energy
        rUU = sum((this % rvU - rU)**2, mask = this % rvU > -9999.0) / n
        rVV = sum((this % rvV - rV)**2, mask = this % rvV > -9999.0) / n
        rWW = sum((this % rvW - rW)**2, mask = this % rvW > -9999.0) / n
        rTKE = 0.5*(rUU + rVV + rWW)

        ! Leave
        deallocate(rvVel)
        
    end function describe

end module mfc_v2
