Module Errores
    Public Const CSSN_NONE = 0
    Public Const CSSN_600 = 1
    Public Const CSSN_800 = 2
    Public Const CSSN_800N = 3
    Public Const CSSN_1000 = 4
    Public Const CSSN_2000 = 5
    Public Const CSSN_2000N = 6
    Public Const CSSN_800E = 7
    Public Const CSSN_800EN = 8
    Public Const CSSN_3000 = 9
    Public Const CSSN_4000 = 10
    Public Const CSSN_800G = 11
    Public Const CSSN_5000 = 12
    Public Const CSSN_IDR = 13  'snapshell
    Public Const CSSN_800DX = 14
    Public Const CSSN_800DXN = 15
    Public Const CSSN_FDA = 16  'snapshell
    Public Const CSSN_WMD = 17    ' SnapShell - Digimark watermar verification only
    Public Const CSSN_TWN = 18    ' SnapShell - General twain camera
    Public Const CSSN_PASS = 19    ' SnapShell - Passport camera
    Public Const CSSN_RTE8K = 20    ' SnapShell - Passport camera
    Public Const CSSN_TWAIN_N = 21
    Public Const CSSN_MAGTEK_STX = 22
    Public Const CSSN_CLBS = 23
    Public Const CSSN_IP = 24
    Public Const CSSN_1000N = 25
    Public Const CSSN_3000DN = 26
    Public Const CSSN_IC_SCAN = 27
    Public Const CSSN_RTE9K = 28
    Public Const CSSN_3100 = 29
    Public Const CSSN_3100N = 30

    Public Const LAST_SCANNER = CSSN_TWN

    ' Scanner return values
    Public Const SLIB_FALSE = 0
    Public Const SLIB_TRUE = 1

    ' Scanner general error types
    Public Const SLIB_ERR_NONE = 1
    Public Const SLIB_ERR_INVALID_SCANNER = -1

    ' Scanning failure definition
    Public Const SLIB_ERR_SCANNER_GENERAL_FAIL = -2
    Public Const SLIB_ERR_CANCELED_BY_USER = -3
    Public Const SLIB_ERR_SCANNER_NOT_FOUND = -4
    Public Const SLIB_ERR_HARDWARE_ERROR = -5
    Public Const SLIB_ERR_PAPER_FED_ERROR = -6
    Public Const SLIB_ERR_SCANABORT = -7
    Public Const SLIB_ERR_NO_PAPER = -8
    Public Const SLIB_ERR_PAPER_JAM = -9
    Public Const SLIB_ERR_FILE_IO_ERROR = -10
    Public Const SLIB_ERR_PRINTER_PORT_USED = -11
    Public Const SLIB_ERR_OUT_OF_MEMORY = -12

    Public Const SLIB_ERR_BAD_WIDTH_PARAM = -2
    Public Const SLIB_ERR_BAD_HEIGHT_PARAM = -3

    Public Const SLIB_ERR_BAD_PARAM = -2

    Public Const SLIB_LIBRARY_ALREADY_INITIALIZED = -13
    Public Const SLIB_ERR_DRIVER_NOT_FOUND = -14
    Public Const GENERAL_ERR_PLUG_NOT_FOUND = -200

    Public Const IMG_ERR_SUCCESS = 0
    Public Const IMG_ERR_FILE_OPEN = -100
    Public Const IMG_ERR_BAD_ANGLE_0 = -101
    Public Const IMG_ERR_BAD_ANGLE_1 = -102
    Public Const IMG_ERR_BAD_DESTINATION = -103
    Public Const IMG_ERR_FILE_SAVE_TO_FILE = -104
    Public Const IMG_ERR_FILE_SAVE_TO_CLIPBOARD = -105
    Public Const IMG_ERR_FILE_OPEN_FIRST = -106
    Public Const IMG_ERR_FILE_OPEN_SECOND = -107
    Public Const IMG_ERR_COMB_TYPE = -108

    Public Const IMG_ERR_BAD_COLOR = -130
    Public Const IMG_ERR_BAD_DPI = -131
    Public Const INVALID_INTERNAL_IMAGE = -132


    ' image saving target definition
    Public Const SAVE_TO_FILE = 0

    ' image rotation angle definitions
    Public Const ANGLE_0 = 0
    Public Function ScannerError(ByVal value As Integer) As Integer
        Select Case value
            Case SLIB_ERR_INVALID_SCANNER 'If Scanner is Invalid
                MsgBox("SCANNER es invalido") 'Prompt Alert Message for Invalid Scanner
                ScannerError = value 'set Error Code as return value
                Exit Function
            Case SLIB_ERR_SCANNER_GENERAL_FAIL 'If Scanner Fails
                MsgBox("SCANNER falla general") 'Prompt Alert Message for Fail
                ScannerError = value 'set Error Code as return value
                Exit Function
            Case SLIB_ERR_CANCELED_BY_USER 'If Initialization Cancelled by User
                MsgBox("Initialization Cancelled by User") 'Prompt Alert Message for Cancellation
                ScannerError = value 'set Error Code as return value
                Exit Function
            Case SLIB_ERR_SCANNER_NOT_FOUND 'If Scanner not found
                MsgBox("SCANNER No se encuentra, verifique si se encuentra conectado.") 'Prompt Error Message for not found
                ScannerError = value 'set Error Code as return value
                Exit Function
            Case SLIB_ERR_HARDWARE_ERROR 'If Hardware Error
                MsgBox("SCANNER HARDWARE ERROR") 'Prompt Error Message for Hardware Error
                ScannerError = value 'Set Error Code as return value
                Exit Function
            Case SLIB_ERR_PAPER_FED_ERROR 'If Paper Feed Error
                MsgBox("SLIB_ERR_PAPER_FED_ERROR") 'Prompt Error Message for Paper Feed Error
                ScannerError = value 'Set Error Code as return value
                Exit Function
            Case SLIB_ERR_SCANABORT 'If Scanner Aborted
                MsgBox("SLIB_ERR_SCANABORT") 'Prompt Error Message for Abortion
                ScannerError = value 'Set Error Code as return value
                Exit Function
            Case SLIB_ERR_NO_PAPER 'If No Paper in Tray
                MsgBox("SLIB_ERR_NO_PAPER") 'Prompt Error Message for No Paper
                ScannerError = value 'Set Error Code as return value
                Exit Function
            Case SLIB_ERR_PAPER_JAM 'If Paper Jam
                MsgBox("SLIB_ERR_PAPER_JAM") 'Prompt Error Message for Jam
                ScannerError = value 'Set error code as return value
                Exit Function
            Case SLIB_ERR_FILE_IO_ERROR 'If File IO Error
                MsgBox("SLIB_ERR_FILE_IO_ERROR") 'Prompt Error Message for File IO
                ScannerError = value 'Set error code as return value
                Exit Function
            Case SLIB_ERR_PRINTER_PORT_USED 'If printer port already used
                MsgBox("SLIB_ERR_PRINTER_PORT_USED") 'Prompt Error Message for already used
                ScannerError = value 'Set error code as return value
                Exit Function
            Case SLIB_ERR_OUT_OF_MEMORY 'If out of memory
                MsgBox("SLIB_ERR_OUT_OF_MEMORY") 'Prompt Error Message for out of memory
                ScannerError = value 'set error code as return value
                Exit Function
            Case SLIB_ERR_BAD_WIDTH_PARAM 'If bad width param
                MsgBox("SLIB_ERR_BAD_WIDTH_PARAM") 'Prompt error message for bad width param
                ScannerError = value 'set error code as return value
                Exit Function
            Case SLIB_ERR_BAD_HEIGHT_PARAM 'If bad height param
                MsgBox("SLIB_ERR_BAD_HEIGHT_PARAM") 'prompt error message for bad height param
                ScannerError = value 'set error code as return value
                Exit Function
            Case SLIB_ERR_BAD_PARAM 'If bad param
                MsgBox("SLIB_ERR_BAD_PARAM") 'prompt error message for bad param
                ScannerError = value 'set error code as return value
                Exit Function
            Case SLIB_LIBRARY_ALREADY_INITIALIZED 'If library already initialized
                MsgBox("SLIB_LIBRARY_ALREADY_INITIALIZED") 'prompt error message for initialization
                ScannerError = value 'set error code as return value
                Exit Function
            Case SLIB_ERR_DRIVER_NOT_FOUND 'If driver not found
                MsgBox("SLIB_ERR_DRIVER_NOT_FOUND") 'prompt error message for not found
                ScannerError = value 'set error code as return value
                Exit Function
            Case GENERAL_ERR_PLUG_NOT_FOUND 'If plug not found
                MsgBox("GENERAL_ERR_PLUG_NOT_FOUND") 'prompt error message for plug not found
                ScannerError = value 'set error code as return value
                Exit Function
            Case -20 'License Key expired
                MsgBox("LICENSE KEY EXPIRED OR SLIB_CONFLICT_WITH_INOUTSCAN_PARAM") 'prompt appropriate error message
                ScannerError = value ' set error code as return value
                Exit Function
            Case -21 'License Key Invalid
                MsgBox("LICENSE INVALID OR SLIB_CONFLICT_WITH_SCAN_SIZE_PARAM") 'prompt error message
                ScannerError = value 'set error code as return value
                Exit Function
            Case -22 'License Key Mismatch
                MsgBox("LICENSE KEY DOES NOT MATCH LIBRARY OR SLIB_NO_SUPPORT_MULTIPLE_DEVICES") 'prompt error message
                ScannerError = value 'set error code as return value
                Exit Function
        End Select
        Return True
    End Function
    'Error Handler for Image Lib SDK - Image Manipulation Operationz
    Public Function ImageError(ByVal value As Integer) As Integer
        Select Case value
            Case IMG_ERR_FILE_OPEN 'if error opening image file
                MsgBox("IMG_ERR_FILE_OPEN") 'prompt error message opening a file
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_BAD_ANGLE_0 'if angle_0 is invalid
                MsgBox("IMG_ERR_BAD_ANGLE_0") 'prompt error message for invalid angle
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_BAD_ANGLE_1 'if angle_1 is invalid
                MsgBox("IMG_ERR_BAD_ANGLE_1") 'prompt error message for invalid angle
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_BAD_DESTINATION 'if destination is invalid
                MsgBox("IMG_ERR_BAD_DESTINATION") 'prompt error message for invalid destination
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_FILE_SAVE_TO_FILE 'if error saving to file
                MsgBox("IMG_ERR_FILE_SAVE_TO_FILE") 'prompt error message for error saving to file
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_FILE_SAVE_TO_CLIPBOARD 'if error saving to clipboard
                MsgBox("IMG_ERR_FILE_SAVE_TO_CLIPBOARD") 'prompt error saving to clipboard
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_FILE_OPEN_FIRST 'if error opening first file
                MsgBox("IMG_ERR_FILE_OPEN_FIRST") 'prompt error opening first file
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_FILE_OPEN_SECOND 'if error opening second file
                MsgBox("IMG_ERR_FILE_OPEN_SECOND") 'prompt error opening second file
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_COMB_TYPE 'if error comb type
                MsgBox("IMG_ERR_COMB_TYPE") 'prompt error message
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_BAD_COLOR 'if color is bad/invalid
                MsgBox("IMG_ERR_BAD_COLOR") 'prompt bad color
                ImageError = value 'set error code to return value
                Exit Function
            Case IMG_ERR_BAD_DPI 'if dpi setting is invalid
                MsgBox("IMG_ERR_BAD_DPI") 'prompt bad setting of dpi
                ImageError = value 'set error code to return value
                Exit Function
            Case INVALID_INTERNAL_IMAGE 'if internal image is invalid
                MsgBox("INVALID_INTERNAL_IMAGE") 'prompt error message for invalid image
                ImageError = value 'set error code to return value
                Exit Function
        End Select
        Return True
    End Function
    'Error Handler for Activation of Twain Scanner on Machine
    Public Function ActivationError(ByVal value As Integer) As Integer
        Select Case value
            Case -1 'if activation key is invalid
                MsgBox("Invalid Activation Key") 'prompt error message
                ActivationError = value 'return error code
                Exit Function
            Case -2 'can't find network card
                MsgBox("Cannot find network card") 'prompt error message
                ActivationError = value 'return error code
                Exit Function
            Case -3 'activation key already used on another PC
                MsgBox("Activation failed. Activation key already used on another PC") 'prompt error message
                ActivationError = value 'return error code
                Exit Function
            Case -4 'comm. error
                MsgBox("Communication error, unable to reach to the Activation server. Check firewall settings") 'prompt error message
                ActivationError = value 'return error code
                Exit Function
            Case -5 'can't create activation
                MsgBox("Cannot create activation") 'prompt error message
                ActivationError = value 'return error code
                Exit Function
            Case -11 'activation failed
                MsgBox("Activation Failed") 'prompt error message
                ActivationError = value 'return error code
                Exit Function
            Case -12 'activation failed
                MsgBox("Activation Failed") 'prompt error message
                ActivationError = value 'return error code
                Exit Function
        End Select
        Return True
    End Function
    'Error Handler for Business card Scanning
    Public Function BizError(ByVal value As Integer) As Integer
        Select Case value
            Case -1 'business card image file not found
                MsgBox("BIZ_FILE_NOT_FOUND") 'prompt error message
                BizError = value 'set error code as return value
                Exit Function
            Case -2 'can't load file
                MsgBox("BIZ_CANNOT_LOAD_FILE") 'prompt error message
                BizError = value 'set error code as return value
                Exit Function
            Case -3 'no lines of data found in business card
                MsgBox("BIZ_NO_LINES") 'prompt error message
                BizError = value 'set error code as return value
                Exit Function
            Case -4 'image not loaded
                MsgBox("BIZ_IMAGE_NOT_LOADED") 'prompt error message
                BizError = value 'set error code as return value
                Exit Function
            Case -5 'invalid field in extracted data
                MsgBox("BIZ_INVALID_FIELD") 'prompt error message
                BizError = value 'set error code to return value
                Exit Function
            Case -6 'no data found
                MsgBox("BIZ_NO_DATA") 'prompt error message
                BizError = value 'set error code as return value
                Exit Function
            Case -7 'last data extracted from business card
                MsgBox("BIZ_LAST_DATA") 'prompt error message
                BizError = value 'set error code as return value
                Exit Function
            Case -8 'if there is more extracted data
                MsgBox("BIZ_NEXT_DATA_EXIST") 'prompt error message
                BizError = value 'set error code as return value
                Exit Function
            Case -10 'invalid version of lib sdk
                MsgBox("BIZ_INVALID_VERSION") 'prompt error message
                BizError = value 'set error code to return value
                Exit Function
        End Select
        Return True
    End Function

End Module
