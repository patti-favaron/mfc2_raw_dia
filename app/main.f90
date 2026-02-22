! mfc2_raw_dia main program
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
program mfc2_raw_dia
    
    use datetime
    use file_helper
    use mfc_v2
    
    implicit none
    
    ! Locals
    integer             :: iRetCode
    character(len=256)  :: sInputPath       ! In Metek form (YYYYMM)
    character(len=16)   :: sFormName
    character(len=256)  :: sOutputFile
    character(len=256)  :: sOutputPath
    character(len=256)  :: sSoniclibFile
    character(len=19)   :: sTimeStamp
    integer             :: iErrCode
    type(data_files)    :: tData
    type(sonic)         :: tSonic
    integer             :: i, j
    integer             :: iLUN
    integer             :: iLUN_Data
    integer             :: iForm
    integer             :: iNumTotal
    integer             :: iNumValid
    integer             :: iNumInvalid
    integer             :: iNumSuspect
    real(4)             :: rVel
    real(4)             :: rScalarVel
    real(4)             :: rR
    real(4)             :: rFractionLowSpeed
    real(4)             :: rDir
    real(4)             :: rW
    real(4)             :: rTKE
    
    ! Check parameters
    if(command_argument_count() /= 2 .and. command_argument_count() /= 3) then
        print *, 'mfc2_raw_dia - Program for converting data to various forms'
        print *
        print *, 'Usage:'
        print *
        print *, '  ./mfc_convert <Input_Path> <Diagnostic_File> [<Output_Path>]'
        print *
        print *, '<Output_Path> is optional: if present data will be also converted'
        print *, 'to SonicLib format.'
        print *
        print *, 'Copyright 2026 by Patrizia Favaron'
        print *, 'This software is open-source, covered by the MIT license'
        print *
        stop
    end if
    call get_command_argument(1, sInputPath)
    call get_command_argument(2, sOutputFile)
    if(command_argument_count() == 3) then
        call get_command_argument(3, sOutputPath)
    else
        sOutputPath = ''
    end if
    
    ! Get the list of file names
    iRetCode = tData % get(sInputPath)
    if(iRetCode /= 0) then
        print *, 'mfc2:: error: Input directory invalid or not read - Return code = ', iRetCode
        stop
    end if
    
    ! Main loop: get files and reformat/process them
    open(newunit=iLUN, file=sOutputFile, status='unknown', action='write')
    write(iLUN, "('date, total, valid, suspect, invalid, vel, vel.scalar, r, slow.fraction, dir, w, tke')")
    do i = 1, size(tData % svFileName)
        if(tData % ivLength(i) > 0) then
        
            ! Get this hour's data ...
            print *, "Processing: ", trim(tData % svFileName(i))
            iRetCode = tSonic % get(tData % svFileName(i), tData % ivLength(i), iNumValid, iNumInvalid, iNumSuspect)
            
            ! ... and process them
            sTimeStamp = get_time_stamp(tData % ivTimeStamp(i))
            iRetCode = tSonic % describe(rVel, rScalarVel, rFractionLowSpeed, rDir, rW, rTKE)
            iNumTotal = iNumValid + iNumSuspect + iNumInvalid
            rR = rVel / rScalarVel
        
        else
        
            sTimeStamp = get_time_stamp(tData % ivTimeStamp(i))
            iNumTotal         =     0
            iNumValid         =     0
            iNumSuspect       =     0
            iNumInvalid       =     0
            rVel              = -9999.9
            rScalarVel        = -9999.9
            rR                = -9999.9
            rFractionLowSpeed = -9999.9
            rDir              = -9999.9
            rW                = -9999.9
            rTKE              = -9999.9
        
        end if
        
        write(iLUN, "(a,4(',',i6),7(',',f10.4))") &
            sTimeStamp, &
            iNumTotal, &
            iNumValid, iNumSuspect, iNumInvalid, &
            rVel, rScalarVel, rFractionLowSpeed, &
            rR, &
            rDir, rW, rTKE
            
        ! If required convert data to SonicLib format
        if(sOutputPath /= '') then
            sOutputFile = trim(sOutputPath) // '/' // trim(basename(tData % svFileName(i)))
            sOutputFile = sOutputFile(1:len_trim(sOutputFile)-1) // '.csv'
            open(newunit=iLUN_Data, file=sOutputFile, status='unknown', action='write')
            write(iLUN_Data, "('time.stamp, u, v, w, t')")
            do j = 1, size(tSonic % rvTimeStamp)
                write(iLUN_Data, "(f7.2,4(',',f8.2))") &
                    tSonic % rvTimeStamp(j), &
                    tSonic % rvU(j), &
                    tSonic % rvV(j), &
                    tSonic % rvW(j), &
                    tSonic % rvT(j)
            end do
            close(iLUN_Data)
        end if
    end do
    close(iLUN)

end program mfc2_raw_dia
