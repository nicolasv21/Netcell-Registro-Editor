Imports System.Drawing
Imports OtiIcaoSDK
Imports OtiIcaoSDK.OtiIcao
Imports System.IO
Imports System.Data
Imports System.Data.SQLite
Imports System.Drawing.Printing



Public Class Form1

    Inherits System.Windows.Forms.Form
    Dim cropBitmap As Bitmap
    Dim cropX As Integer
    Dim cropY As Integer
    Dim cropWidth As Integer
    Dim cropHeight As Integer
    Public cropPen As Pen
    Public cropPenSize As Integer = 2
    Public cropDashStyle As Drawing2D.DashStyle = Drawing2D.DashStyle.Solid
    Public cropPenColor As Color = Color.Aquamarine
    Public c As Cursors
    Dim opcion3, opcion1 As Boolean
    Dim leido As Boolean = False
    Public Const BLACKNESS = &H42
    Public Const DSTINVERT = &H550009
    Public Const CAPTUREBLT = &H40000000
    Public Const MERGECOPY = &HC000CA
    Public Const MERGEPAINT = &HBB0226
    Public Const NOMIRRORBITMAP = &H80000000
    Public Const NOTSRCCOPY = &H330008
    Public Const NOTSRCERASE = &H1100A6
    Public Const PATCOPY = &HF00021
    Public Const PATINVERT = &H5A0049
    Public Const PATPAINT = &HFB0A09
    Public Const SRCCOPY = &HCC0020
    Public Const SRCAND = &H8800C6
    Public Const SRCERASE = &H440328
    Public Const SRCINVERT = &H660046
    Public Const SRCPAINT = &HEE0086
    Public Const WHITENESS = &HFF0062
    Public Declare Function BitBlt Lib "gdi32" Alias "BitBlt" (ByVal hDestDC As Integer, ByVal x As Integer, ByVal y As Integer, ByVal nWidth As Integer, ByVal nHeight As Integer, ByVal hSrcDC As Integer, ByVal xSrc As Integer, ByVal ySrc As Integer, ByVal dwRop As Integer) As Integer
    Public Declare Function GetDesktopWindow Lib "user32" () As IntPtr
    Dim GrayscaleMatrix As Imaging.ColorMatrix = New Imaging.ColorMatrix(New Single()() _
              {New Single() {0.3, 0.3, 0.3, 0, 0}, _
               New Single() {0.59, 0.59, 0.59, 0, 0}, _
               New Single() {0.11, 0.11, 0.11, 0, 0}, _
               New Single() {0, 0, 0, 1, 0}, _
               New Single() {0, 0, 0, 0, 1}})
    Dim ImageAttributes As New Imaging.ImageAttributes
    Private Declare Function GetDC Lib "user32" (ByVal hwnd As Int32) As Int32
    Private Declare Function ReleaseDC Lib "user32" (ByVal hwnd As Int32, ByVal hdc As Int32) As Int32
    Dim MyDataGridViewPrinter As DataGridViewPrinter
    Dim CONNECTION_STR As String = "Data Source=c:\Netcell\DB\CedulasDB.db;Version=3;"

    Dim ventana_regresar As String
    Dim ventana_actual As String
    Dim Ventana_redirectGlobal As String
    Dim personalID As String = String.Empty
    Private start_time As DateTime
    Private stop_time As DateTime
    Dim elapsed_time As TimeSpan
    Private WSQ_library_native_methods As WSQImageLibraryNativeMethods
    Dim hbitmap As IntPtr
    Dim tableiniatilize As Boolean = False
    Dim huellagemalto() As Byte = New Byte() {0}
    Dim messageText As String = String.Empty

    Dim firstName As String = String.Empty
    Dim lastName As String = String.Empty
    Dim dateOfBirth As String = String.Empty
    Dim validUntil As String = String.Empty
    Dim nationality As String = String.Empty
    Dim sex As String = String.Empty
    Dim documentNumber As String = String.Empty
    Dim firstFingerIcaoIndex As Integer = -1
    Dim secondFingerIcaoIndex As Integer = -1
    Dim firstFingerTemplate As Byte() = Nothing
    Dim secondFingerTemplate As Byte() = Nothing
    Dim photo As Byte() = Nothing
    Dim fingerIcao As Byte() = Nothing
    Dim signature As Byte() = Nothing
    Dim chipId As Byte() = Nothing
    Dim dateOfBirth_11 As String = String.Empty
    Dim personalID_11 As String = String.Empty
    Dim checkDigit1 As String = String.Empty
    Dim checkDigit2 As String = String.Empty
    Dim checkDigit3 As String = String.Empty
    Dim checkDigit4 As String = String.Empty
    Dim documentCode As String = String.Empty
    Dim issuingState As String = String.Empty
    Dim holderName As String = String.Empty
    Dim holderName_11 As String = String.Empty
    Dim placeOfBirth As String = String.Empty
    Dim telephone As String = String.Empty
    Dim profession As String = String.Empty
    Dim address As String = String.Empty


    ' Apis  
    Private Declare Function SendMessage _
        Lib "user32" _
        Alias "SendMessageA" ( _
            ByVal hwnd As Integer, _
            ByVal wMsg As Integer, _
            ByVal wParam As Integer, _
            ByRef lParam As Object) As Integer

    Private Declare Function ReleaseCapture Lib "user32" () As Integer
    ' constantes / variables  
    Private Const WM_NCLBUTTONDOWN As Integer = &HA1
    Private Const HTCAPTION As Integer = 2


    Dim result As Integer
    Dim m_DirPath, m_ScanPath, m_DupScanPath As String

    Private Sub Form1_KeyDown( _
        ByVal sender As Object, _
        ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        If e.KeyCode = Keys.Escape Then Me.Close()
    End Sub

    Private Sub Form1_MouseDown( _
        ByVal sender As Object, _
        ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub




    Sub resise_Formulario(ByVal frm As Form)
        'cambiar el puntero  
        Me.Cursor = Cursors.NoMove2D
        Me.Width = Windows.Forms.Cursor.Position.X
        Dim ret As Integer = SendMessage( _
                                    frm.Handle.ToInt32, _
                                    WM_NCLBUTTONDOWN, _
                                    HTCAPTION, 0)
        '  reestablecer el cursor al soltar  
        Me.Cursor = Cursors.Default
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
        Button1.Visible = True
        Button9.Visible = False
    End Sub

    Sub Mover_Formulario(ByVal frm As Form)
        ReleaseCapture()
        'cambiar el puntero  
        Me.Cursor = Cursors.NoMove2D
        Dim ret As Integer = SendMessage( _
                                    frm.Handle.ToInt32, _
                                    WM_NCLBUTTONDOWN, _
                                    HTCAPTION, 0)
        '  reestablecer el cursor al soltar  
        Me.Cursor = Cursors.Default

        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
        Button1.Visible = True
        Button9.Visible = False
    End Sub

    Private Function MostrarError(ByRef messageText As String)
        If messageText = "Archivo OTI ID no se encuentra. Por favor, suministrar ID Personal e intentar de nuevo." Then
            abrir("id_file")
            Return False
        ElseIf messageText = "OTI Saturn reader communication service: OTI_SATURN_CANNOT_FIND_CARD (C0000002)" & vbCrLf & "Cannot find card." Then
            MessageBox.Show("Chip No Valido!")
        ElseIf messageText = "OTI ID file not found. Please, supply Personal ID and try again." Then
            MessageBox.Show("Ingrese el numero de cédula!")
        ElseIf messageText = "OTI Saturn reader communication service: OTI_SATURN_SELECT_CHANNEL_ERROR (C0000004)" & vbCrLf & "Cannot select channel: Contactless." Then
            Application.Restart()
        ElseIf messageText = "OTI Saturn reader communication service: OTI_SATURN_CANNOT_FIND_READER (C0000000)" & vbCrLf & "Cannot find OTI Saturn reader." Then
            Application.Restart()
        ElseIf messageText = "APDU TLV files processor service: OTI_TLVAPDU_ICAO_ERROR_CANNOT_OPEN_SEC_CHNL (C0000003)" & vbCrLf & "Cannot open secure channel: APDU [BAC Mutual Authentication] Wrong response: 6300." Then
            abrir("id_file")
            Return False
        ElseIf messageText = "APDU TLV files processor service: OTI_TLVAPDU_ICAO_ERROR_CANNOT_OPEN_SEC_CHNL (C0000003)" & vbCrLf & "Cannot open secure channel: APDU [BAC Mutual Authentication] Wrong response: 6982." Then
            abrir("id_file")
            Return False
        ElseIf messageText = "OTI ECUID PICC processor service: OTI_TLVAPDU_APPLICATION_ERROR (FFFFFFFF)" & vbCrLf & "La matriz de origen no es suficientemente larga. Compruebe srcIndex, la longitud y los límites inferiores de la matriz." Then
            abrir("id_file")
            Return False
        ElseIf messageText = "OTI ECUID PICC processor service: OTI_TLVAPDU_APPLICATION_ERROR (FFFFFFFF)" & vbCrLf & "Object reference not set to an instance of an object." Then
            Application.Restart()
        End If
        Return True
    End Function
    Private Function botones(Optional ByVal btn1 As Boolean = False, Optional ByVal btn1_text As String = "", Optional ByVal btn2 As Boolean = False, Optional ByVal btn2_text As String = "", Optional ByVal btn3 As Boolean = False, Optional ByVal btn3_text As String = "", Optional ByVal btn4 As Boolean = False, Optional ByVal btn4_text As String = "")

        btn_3.Visible = False
        btn_4.Visible = False
    

        If btn3 = True Then
            btn_3.Text = btn3_text
            btn_3.Visible = True
        End If
        If btn4 = True Then
            btn_4.Text = btn4_text
            btn_4.Visible = True
        End If
        Return True
    End Function

    Private Function abrir(Optional ByVal Ventana_solicitada As String = Nothing, Optional ByVal Ventana_redirect As String = Nothing)

        Header.Visible = False   ' Ventana de solicitud de cedula en el lector.
        id_file.Visible = False  ' Ventana de solicitud de ID FILE.
        Error_1.Visible = False  ' Ventana de error de conexion del lector de cédulas.
        Datos.Visible = False    ' Ventana que muestra todos los datos del chip.
        fail.Visible = False     ' Ventana de error de chip id.
        Report.Visible = False     ' Ventana de Reporte.
        imprimir.Visible = False
        Reporte_interno.Visible = False




        If ventana_actual <> Ventana_solicitada Or ventana_actual <> "Lectura" Then


            If Ventana_solicitada = "Error_1" Then
              
                btn_2.Visible = False  'reporte
                btn_3.Visible = False  'limpiar
                btn_4.Visible = False   'guardar
                btn_5.Visible = True  'cerrar
                lbl_footer_1.Visible = False
                lbl_footer_2.Visible = False
                Error_1.Visible = True
                Error_1.Dock = DockStyle.Fill
            End If
            If Ventana_solicitada = "Header" Then
            
                btn_4.Visible = False  'guardar
                btn_3.Visible = False  'limpiar
                btn_2.Visible = True  'reporte


                btn_5.Visible = False  'cerrar
                Me.lbl_footer_1.Text = "0-10 Segundos"
                Me.lbl_footer_2.Text = "TIEMPO APROXIMADO DE LECTURA"
                lbl_footer_1.Visible = True
                lbl_footer_2.Visible = True
                SplitContainer1.Panel1Collapsed = True
                SplitContainer1.Panel2Collapsed = False
                Header.Visible = True
                Header.Dock = DockStyle.Fill
            End If

            If Ventana_solicitada = "Datos" Then
             
                btn_2.Visible = True  'reporte
                btn_3.Visible = True  'limpiar
                btn_4.Visible = True  'guardar
                btn_5.Visible = False  'cerrar
                lbl_footer_1.Visible = True
                lbl_footer_2.Visible = True
                Datos.Visible = True
                Datos.Dock = DockStyle.Fill
            End If
            If Ventana_solicitada = "fail" Then
            
                btn_2.Visible = False  'reporte
                btn_3.Visible = False  'limpiar
                btn_4.Visible = False  'guardar
                btn_5.Visible = False  'cerrar
                lbl_footer_1.Visible = False
                lbl_footer_2.Visible = False
                fail.Visible = True
                fail.Dock = DockStyle.Fill
            End If
            If Ventana_solicitada = "id_file" Then
             
                btn_2.Visible = False  'reporte
                btn_3.Visible = False  'limpiar
                btn_4.Visible = False  'guardar
                btn_5.Visible = False  'cerrar
                lbl_footer_1.Visible = False
                lbl_footer_2.Visible = False
                id_file.Visible = True
                id_file.Dock = DockStyle.Fill
                personal_id.Focus()

            End If

            If Ventana_solicitada = "imprimir" Then

                Application.DoEvents()
              
                btn_2.Visible = False  'reporte
                btn_3.Visible = False  'limpiar
                btn_4.Visible = False   'guardar
                lbl_footer_1.Visible = False
                lbl_footer_2.Visible = False
                Me.CedulasTableAdapter1.Fill(Me.DBDataSet1.cedulas)
                Me.ReportViewer1.RefreshReport()
                imprimir.Visible = True
                imprimir.Dock = DockStyle.Fill
            End If
            If Ventana_solicitada = "Reporte_interno" Then

                Application.DoEvents()
              
                btn_2.Visible = False  'reporte
                btn_3.Visible = False  'limpiar
                btn_4.Visible = False  'guardar
                btn_5.Visible = False  'cerrar
                lbl_footer_1.Visible = False
                lbl_footer_2.Visible = False
                Reporte_interno.Visible = True
                Reporte_interno.Dock = DockStyle.Fill
            End If
            Ventana_redirectGlobal = ventana_regresar
            If Ventana_redirect <> "" Then
                ventana_regresar = Ventana_redirect 'Guardo la ventana actual 
            Else
                ventana_regresar = ventana_actual 'Guardo la ventana actual 
            End If

            ventana_actual = Ventana_solicitada  'Guardo la ventana nueva

            Return True

        End If
        Return False

    End Function
   
  

    
    Private Function GetImage(ByVal filePath As String) As Image
        Dim fs As FileStream 'variable for filestream
        Dim tmp As Image 'temporary variable to hold Image

        If (Not File.Exists(filePath)) Then 'if file doesn't exist
            Return Nothing 'return null
        End If

        If (filePath.Contains(".ejpg")) Then 'if file path extension is invalid
            Return Nothing
        End If

        fs = New FileStream(filePath, FileMode.Open) 'fetch file stream boject corresponding to filepath
        tmp = Image.FromStream(fs) 'fetch image from stream
        fs.Close() 'close filestream
        fs = Nothing 'set filestream handle to null
        Return tmp
    End Function
    Private Sub Form1_Load(ByVal sender As Object, ByVal e As EventArgs) Handles MyBase.Load
        Try

            CheckForIllegalCrossThreadCalls = False
            'TODO: This line of code loads data into the 'DBDataSet2.cedulas' table. You can move, or remove it, as needed.
            Me.CedulasTableAdapter.Fill(Me.DBDataSet2.cedulas)
            'TODO: This line of code loads data into the 'DBDataSet1.cedulas' table. You can move, or remove it, as needed.
            Me.CedulasTableAdapter1.Fill(Me.DBDataSet1.cedulas)

            ventana_actual = "Header"

            Me.ReportViewer1.RefreshReport()
            tabla_settings()
            sysRecientes()
            abrir("Datos")
            Dim InitSDK As Boolean

            If (result < 0 And result <> -13) Then
                ScannerError(result)
                InitSDK = False
                Exit Sub
            Else
                InitSDK = True
            End If
            m_DirPath = "c:\Netcell\Working" 'set default directory path
            m_ScanPath = m_DirPath & "BZ_ScannedImage.jpg" 'set name of scanned image
            m_DupScanPath = m_DirPath & "BZ_ScannedImage-back.jpg" 'set name of back scanned image

        Catch ex As Exception

        End Try
    End Sub
    Private Sub otiIcao1_OnFileBlockProcessed()
        Application.DoEvents()
    End Sub
    Private Sub otiIcao1_OnFileStartProcess(ByVal dg_tag As Byte, ByVal block_count As Integer)
        Application.DoEvents()
    End Sub
    Private Sub leer_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
    
    End Sub
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)




    End Sub
    Private Sub btn_3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If btn_3.Text = "CERRAR" Then
            MyBase.Close()
            Application.Exit()
        End If
        If btn_3.Text = "REGRESAR" Then
            If ventana_actual = "Datos" Then
                abrir("Header")
            Else
                ' abrir(Me.ventana_regresar)
            End If
        End If

    End Sub









    Private Sub Datos_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Datos.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub



    Private Sub CreateDatabase()
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Try
            objConn = New SQLiteConnection(CONNECTION_STR & "New=True;")
            objConn.Open()
            objCommand = objConn.CreateCommand()
            objCommand.CommandText = "CREATE TABLE cedulas (id integer primary key, cedula varchar(10), nombres TEXT, apellidos TEXT, nacionalidad TEXT, sexo varchar(2), lugar_nacimiento TEXT, fecha_nacimiento TEXT, profesion TEXT, telefono TEXT,direccion TEXT,fecha_expiracion TEXT,numero_plastico TEXT,numero_chip TEXT);"
            objCommand.ExecuteNonQuery()
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try
    End Sub


    Private Sub SaveRecord()
        Dim lado2var As Byte() = Nothing
        Dim lado1var As Byte() = Nothing
        Dim huellasensor As Byte() = Nothing
        Try
            Dim oStream As New MemoryStream()
            LADOA.Image.Save(oStream, System.Drawing.Imaging.ImageFormat.Jpeg)
            Dim oBytes(oStream.Length - 1) As Byte
            oStream.Position = 0
            oStream.Read(oBytes, 0, Convert.ToInt32(oStream.Length))
            oStream.Close()
            lado1var = oBytes
        Catch ex As Exception

        End Try

        Try
            Dim oStream2 As New MemoryStream()
            LADOB.Image.Save(oStream2, System.Drawing.Imaging.ImageFormat.Jpeg)
            Dim oBytes2(oStream2.Length - 1) As Byte
            oStream2.Position = 0
            oStream2.Read(oBytes2, 0, Convert.ToInt32(oStream2.Length))
            oStream2.Close()
            lado2var = oBytes2
        Catch ex As Exception

        End Try

        'Try
        'Dim oStream3 As New MemoryStream()
        'd_sensor.Image.Save(oStream3, System.Drawing.Imaging.ImageFormat.Jpeg)
        'Dim oBytes3(oStream3.Length - 1) As Byte
        'oStream3.Position = 0
        'oStream3.Read(oBytes3, 0, Convert.ToInt32(oStream3.Length))
        ' oStream3.Close()
        'huellasensor = oBytes3
        'Catch ex As Exception

        ' End Try

        Try
            Dim oStream3 As New MemoryStream()
            d_foto.Image.Save(oStream3, System.Drawing.Imaging.ImageFormat.Jpeg)
            Dim oBytes3(oStream3.Length - 1) As Byte
            oStream3.Position = 0
            oStream3.Read(oBytes3, 0, Convert.ToInt32(oStream3.Length))
            oStream3.Close()
            photo = oBytes3
        Catch ex As Exception

        End Try
        Dim d_fecha_save As String
        d_fecha_save = Now.ToString("G")
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Try
            objConn = New SQLiteConnection(CONNECTION_STR)
            objConn.Open()
            objCommand = objConn.CreateCommand()
            objCommand.CommandText = "INSERT INTO cedulas (id, cedula, nombres , apellidos,nacionalidad,sexo,lugar_nacimiento,fecha_nacimiento,profesion,telefono,direccion,fecha_expiracion,numero_plastico,numero_chip,foto,fecha_save,ladoA,ladoB,observaciones,email,telefono2,celular2,direccion2) VALUES ('" & lbl_id.Text & "','" & d_cedula.Text & "','" & d_nombres.Text & "','" & d_apellidos.Text & "','" & d_nacionalidad.Text & "','" & d_sexo.Text & "','" & d_lugar_nac.Text & "','" & d_fecha_nac.Text & "','" & d_profesion.Text & "','" & d_telefono.Text & "','" & d_direccion.Text & "','" & d_fecha_expiracion.Text & "','" & d_numero_plastico.Text & "','" & d_numero_chip.Text & "',@photo,'" & d_fecha_save & "',@ladoA,@ladoB,'" & campo1.Text & "','" & campo2.Text & "','" & campo4.Text & "','" & campo5.Text & "','" & campo3.Text & "');"
            objCommand.Parameters.Add("@photo", DbType.Binary, 20).Value = photo
            objCommand.Parameters.Add("@ladoA", DbType.Binary, 20).Value = lado1var
            objCommand.Parameters.Add("@ladoB", DbType.Binary, 20).Value = lado2var
            ' objCommand.Parameters.Add("@huellasensor", DbType.Binary, 20).Value = huellasensor
            ' If (huellagemalto.Length > 1) Then
            'objCommand.Parameters.Add("@huella", DbType.Binary, 20).Value = huellagemalto
            'Else
            'objCommand.Parameters.Add("@huella", DbType.Binary, 20).Value = fingerIcao
            'End If
            ' objCommand.Parameters.Add("@firma", DbType.Binary, 20).Value = signature
            objCommand.ExecuteNonQuery()
            clear()
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try
    End Sub

    Private Sub editRecord(ByVal idrecord As String)
        Dim lado2var() As Byte = New Byte() {0}
        Dim lado1var() As Byte = New Byte() {0}

        Dim huellasensor() As Byte = New Byte() {0}
        Try
            Dim oStream As New MemoryStream()
            LADOA.Image.Save(oStream, System.Drawing.Imaging.ImageFormat.Jpeg)
            Dim oBytes(oStream.Length - 1) As Byte
            oStream.Position = 0
            oStream.Read(oBytes, 0, Convert.ToInt32(oStream.Length))
            oStream.Close()
            lado1var = oBytes
        Catch ex As Exception

        End Try

        Try
            Dim oStream2 As New MemoryStream()
            LADOB.Image.Save(oStream2, System.Drawing.Imaging.ImageFormat.Jpeg)
            Dim oBytes2(oStream2.Length - 1) As Byte
            oStream2.Position = 0
            oStream2.Read(oBytes2, 0, Convert.ToInt32(oStream2.Length))
            oStream2.Close()
            lado2var = oBytes2
        Catch ex As Exception

        End Try

        Try
            Dim oStream3 As New MemoryStream()
            d_foto.Image.Save(oStream3, System.Drawing.Imaging.ImageFormat.Jpeg)
            Dim oBytes3(oStream3.Length - 1) As Byte
            oStream3.Position = 0
            oStream3.Read(oBytes3, 0, Convert.ToInt32(oStream3.Length))
            oStream3.Close()
            photo = oBytes3
        Catch ex As Exception

        End Try

        Dim d_fecha_modificado As String
        d_fecha_modificado = Now.ToString("G")
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Dim more1, more2, more3, cierre As String

        Try
            objConn = New SQLiteConnection(CONNECTION_STR)
            objConn.Open()
            objCommand = objConn.CreateCommand()
            objCommand.CommandText = "UPDATE  cedulas set cedula='" & d_cedula.Text & "' , nombres='" & d_nombres.Text & "' , apellidos='" & d_apellidos.Text & "' ,nacionalidad='" & d_nacionalidad.Text & "' ,sexo='" & d_sexo.Text & "' ,lugar_nacimiento='" & d_lugar_nac.Text & "' ,fecha_nacimiento='" & d_fecha_nac.Text & "' ,profesion='" & d_profesion.Text & "' ,telefono='" & d_telefono.Text & "',telefono2='" & campo4.Text & "',celular2='" & campo5.Text & "' ,direccion2='" & campo3.Text & "',email='" & campo2.Text & "',observaciones='" & campo1.Text & "', direccion='" & d_direccion.Text & "' ,fecha_expiracion='" & d_fecha_expiracion.Text & "' ,numero_plastico='" & d_numero_plastico.Text & "' ,numero_chip='" & d_numero_chip.Text & "' ,foto=@photo ,firma=@firma ,huella=@huella ,fecha_modificado='" & d_fecha_modificado & "' ,fecha_comparacion='" & v_6.Text & "' "
            If lado1var.Length > 1 Then
                more1 = ",ladoA=@ladoA "
            End If
            If lado2var.Length > 1 Then
                more2 = ",ladoB=@ladoB "
            End If
            If huellasensor.Length > 1 Then
                more3 = ",huellaSensor=@huellaSensor "
            End If
            cierre = "  WHERE id='" + CStr(idrecord) + "'"
            objCommand.CommandText = objCommand.CommandText + more1 + more2 + more3 + cierre
            objCommand.Parameters.Add("@photo", DbType.Binary, 20).Value = photo
            If lado1var.Length > 1 Then
                objCommand.Parameters.Add("@ladoA", DbType.Binary, 20).Value = lado1var
            End If
            If lado2var.Length > 1 Then
                objCommand.Parameters.Add("@ladoB", DbType.Binary, 20).Value = lado2var
            End If
            If huellasensor.Length > 1 Then
                objCommand.Parameters.Add("@huellasensor", DbType.Binary, 20).Value = huellasensor
            End If
            If (huellagemalto.Length > 1) Then
                objCommand.Parameters.Add("@huella", DbType.Binary, 20).Value = huellagemalto
            Else
                objCommand.Parameters.Add("@huella", DbType.Binary, 20).Value = fingerIcao
            End If
            objCommand.Parameters.Add("@firma", DbType.Binary, 20).Value = signature
            objCommand.ExecuteNonQuery()
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try
    End Sub

    Private Sub Button1_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs)
        ReportViewer1.Reset()
        Dim objConn As SQLiteConnection
        Dim objCommand As SQLiteCommand
        objConn = New SQLiteConnection(CONNECTION_STR)
        objConn.Open()
        objCommand = objConn.CreateCommand()
        objCommand.CommandText = "SELECT * FROM cedulas"
        Dim SQLiteDataAdapter = New SQLiteDataAdapter(objCommand)
        Dim DataSet = New DataSet()
        Dim DataTable = New DataTable()
        SQLiteDataAdapter.Fill(DataSet)
        DataTable = DataSet.Tables(0)
        objConn.Close()
        Dim ReportDataSource = New Microsoft.Reporting.WinForms.ReportDataSource
        ReportDataSource.Name = "Report1"
        ReportDataSource.Value = DataTable
        ReportViewer1.LocalReport.DataSources.Clear()
        ReportViewer1.LocalReport.DataSources.Add(ReportDataSource)
        ReportViewer1.RefreshReport()
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        CreateDatabase()
        SaveRecord()
    End Sub

    Private Sub Header_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Header.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub Button1_Click_5(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_2.Click
        abrir("imprimir")
    End Sub

    Private Sub Button2_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs)
        abrir("Reporte_interno")
    End Sub

    Private Sub btn_4_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_4.Click
        If btn_4.Text = "GUARDAR" Then
            SaveRecord()
            MessageBox.Show("Registro Guardado exitosamente")
        End If
        If btn_4.Text = "GUARDAR CAMBIOS" Then
            editRecord(id_reporte_interno)
            clear()
            MessageBox.Show("Registro Editada exitosamente")
        End If
        sysRecientes()
    End Sub

    Function idsys()
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Dim objReader As SQLiteDataReader
        Try
            objConn = New SQLiteConnection(CONNECTION_STR)
            objConn.Open()
            objCommand = objConn.CreateCommand()
            objCommand.CommandText = "SELECT  id FROM cedulas WHERE ID =(SELECT MAX(ID) FROM cedulas)"
            objReader = objCommand.ExecuteReader()
            While (objReader.Read())
                Dim id As Integer = CInt(objReader("id"))
                id = id + 1
                Me.lbl_id.Text = id.ToString.PadLeft(5, "0"c)
            End While
        Catch ex As Exception
            MessageBox.Show("An error has occurred: " & ex.Message)
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try
        If Me.lbl_id.Text = "" Then
            Me.lbl_id.Text = "00001"
        End If
    End Function


    Private Sub btn_1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        If ventana_actual = "id_file" Then
            If personal_id.Text.Length < 10 Then
                id_file_error.Text = "El número de cédula no tiene 10 caracteres"
                id_file_error.Visible = True
            Else

                btn_4.Text = "GUARDAR"
                id_file_error.Visible = False
                abrir("Datos")
                start_time = Now
                Panel14.Visible = True

                Panel14.Visible = False
                stop_time = Now
                elapsed_time = stop_time.Subtract(start_time)
                Me.lbl_footer_1.Text = elapsed_time.TotalSeconds.ToString("0.000") & " Segundos"
                Me.lbl_footer_2.Text = "TIEMPO DE LECTURA"
            End If
            Exit Sub
        End If
        If ventana_actual = "fail" Then
            btn_4.Text = "GUARDAR"
            id_file_error.Visible = False
            abrir("Datos")
            start_time = Now
            Panel14.Visible = True

            Panel14.Visible = False
            stop_time = Now
            elapsed_time = stop_time.Subtract(start_time)
            Me.lbl_footer_1.Text = elapsed_time.TotalSeconds.ToString("0.000") & " Segundos"
            Me.lbl_footer_2.Text = "TIEMPO DE LECTURA"

            Exit Sub

        End If


        If ventana_actual = "Datos" Then
            clear()
            btn_4.Text = "GUARDAR"
            start_time = Now
            Panel14.Visible = True

            Panel14.Visible = False
            stop_time = Now
            elapsed_time = stop_time.Subtract(start_time)
            Me.lbl_footer_1.Text = elapsed_time.TotalSeconds.ToString("0.000") & " Segundos"
            Me.lbl_footer_2.Text = "TIEMPO DE LECTURA"

            Exit Sub
        Else
            idsys()
            abrir("Datos")
        End If

    End Sub



    Private Sub txt_cedula_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_cedula.GotFocus
        getfocus(txt_cedula)
    End Sub
    Private Sub txt_cedula_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_cedula.LostFocus
        lestfocus(txt_cedula)
    End Sub
    Private Sub txt_cedula_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_cedula.TextChanged
        buscar()
    End Sub

    Private Sub txt_nombres_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_nombres.GotFocus
        getfocus(txt_nombres)
    End Sub
    Private Sub txt_nombres_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_nombres.LostFocus
        lestfocus(txt_nombres)
    End Sub
    Private Sub txt_nombres_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_nombres.TextChanged
        buscar()
    End Sub

    Private Sub txt_apellidos_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_apellidos.GotFocus
        getfocus(txt_apellidos)
    End Sub
    Private Sub txt_apellidos_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_apellidos.LostFocus
        lestfocus(txt_apellidos)
    End Sub
    Private Sub txt_apellidos_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_apellidos.TextChanged
        buscar()
    End Sub

    Private Sub txt_nacionalidad_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_nacionalidad.GotFocus
        getfocus(txt_nacionalidad)
    End Sub
    Private Sub txt_nacionalidad_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_nacionalidad.LostFocus
        lestfocus(txt_nacionalidad)
    End Sub
    Private Sub txt_nacionalidad_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_nacionalidad.TextChanged
        buscar()
    End Sub

    Private Sub txt_sexo_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_sexo.GotFocus
        getfocus(txt_sexo)
    End Sub
    Private Sub txt_sexo_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_sexo.LostFocus
        lestfocus(txt_sexo)
    End Sub
    Private Sub txt_sexo_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_sexo.TextChanged
        buscar()
    End Sub

    Private Sub txt_lugar_nacimiento_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_lugar_nacimiento.GotFocus
        getfocus(txt_lugar_nacimiento)
    End Sub
    Private Sub txt_lugar_nacimiento_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_lugar_nacimiento.LostFocus
        lestfocus(txt_lugar_nacimiento)
    End Sub
    Private Sub txt_lugar_nacimiento_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_lugar_nacimiento.TextChanged
        buscar()
    End Sub

    Private Sub txt_fecha_nacimiento_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_fecha_nacimiento.GotFocus
        getfocus(txt_fecha_nacimiento)
    End Sub
    Private Sub txt_fecha_nacimiento_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_fecha_nacimiento.LostFocus
        lestfocus(txt_fecha_nacimiento)
    End Sub
    Private Sub txt_fecha_nacimiento_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_fecha_nacimiento.TextChanged
        buscar()
    End Sub

    Private Sub txt_profesion_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_profesion.GotFocus
        getfocus(txt_profesion)
    End Sub
    Private Sub txt_profesion_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_profesion.LostFocus
        lestfocus(txt_profesion)
    End Sub
    Private Sub txt_profesion_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_profesion.TextChanged
        buscar()
    End Sub

    Private Sub txt_telefono_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_telefono.GotFocus
        getfocus(txt_telefono)
    End Sub
    Private Sub txt_telefono_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_telefono.LostFocus
        lestfocus(txt_telefono)
    End Sub
    Private Sub txt_telefono_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_telefono.TextChanged
        buscar()
    End Sub

    Private Sub txt_direccion_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_direccion.GotFocus
        getfocus(txt_direccion)
    End Sub
    Private Sub txt_direccion_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_direccion.LostFocus
        lestfocus(txt_direccion)
    End Sub
    Private Sub txt_direccion_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_direccion.TextChanged
        buscar()
    End Sub

    Private Sub txt_fecha_expiracion_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_fecha_expiracion.GotFocus
        getfocus(txt_fecha_expiracion)
    End Sub
    Private Sub txt_fecha_expiracion_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_fecha_expiracion.LostFocus
        lestfocus(txt_fecha_expiracion)
    End Sub
    Private Sub txt_fecha_expiracion_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_fecha_expiracion.TextChanged
        buscar()
    End Sub

    Private Sub txt_numero_plastico_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_numero_plastico.GotFocus
        getfocus(txt_numero_plastico)
    End Sub
    Private Sub txt_numero_plastico_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_numero_plastico.LostFocus
        lestfocus(txt_numero_plastico)
    End Sub
    Private Sub txt_numero_plastico_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_numero_plastico.TextChanged
        buscar()
    End Sub

    Private Sub txt_numero_chip_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_numero_chip.GotFocus
        getfocus(txt_numero_chip)
    End Sub
    Private Sub txt_numero_chip_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txt_numero_chip.LostFocus
        lestfocus(txt_numero_chip)
    End Sub
    Private Sub txt_numero_chip_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txt_numero_chip.TextChanged
        buscar()
    End Sub

    Function getfocus(ByVal textbox_name As TextBox)
        If textbox_name.Text = textbox_name.Tag Then
            textbox_name.ForeColor = Color.Black
            textbox_name.Text = ""
        End If
        Return True
    End Function
    Function lestfocus(ByVal textbox_name As TextBox)
        If textbox_name.Text = Nothing Then
            textbox_name.ForeColor = Color.Gray
            textbox_name.Text = textbox_name.Tag
        End If
        Return True
    End Function
    Function buscar()
        Dim cedula_param As String = "%" & txt_cedula.Text & "%"
        Dim nombres_param As String = "%" & txt_nombres.Text & "%"
        Dim apellidos_param As String = "%" & txt_apellidos.Text & "%"
        Dim nacionalidad_param As String = "%" & txt_nacionalidad.Text & "%"
        Dim sexo_param As String = "%" & txt_sexo.Text & "%"
        Dim lugar_nacimiento_param As String = "%" & txt_lugar_nacimiento.Text & "%"
        Dim fecha_nacimiento_param As String = "%" & txt_fecha_nacimiento.Text & "%"
        Dim profesion_param As String = "%" & txt_profesion.Text & "%"
        Dim telefono_param As String = "%" & txt_telefono.Text & "%"
        Dim direccion_param As String = "%" & txt_direccion.Text & "%"
        Dim fecha_expiracion_param As String = "%" & txt_fecha_expiracion.Text & "%"
        Dim numero_plastico_param As String = "%" & txt_numero_plastico.Text & "%"
        Dim numero_chip_param As String = "%" & txt_numero_chip.Text & "%"
        If cedula_param = "%" & txt_cedula.Tag & "%" Then cedula_param = "%%"
        If nombres_param = "%" & txt_nombres.Tag & "%" Then nombres_param = "%%"
        If apellidos_param = "%" & txt_apellidos.Tag & "%" Then apellidos_param = "%%"
        If nacionalidad_param = "%" & txt_nacionalidad.Tag & "%" Then nacionalidad_param = "%%"
        If sexo_param = "%" & txt_sexo.Tag & "%" Then sexo_param = "%%"
        If lugar_nacimiento_param = "%" & txt_lugar_nacimiento.Tag & "%" Then lugar_nacimiento_param = "%%"
        If fecha_nacimiento_param = "%" & txt_fecha_nacimiento.Tag & "%" Then fecha_nacimiento_param = "%%"
        If profesion_param = "%" & txt_profesion.Tag & "%" Then profesion_param = "%%"
        If telefono_param = "%" & txt_telefono.Tag & "%" Then telefono_param = "%%"
        If direccion_param = "%" & txt_direccion.Tag & "%" Then direccion_param = "%%"
        If fecha_expiracion_param = "%" & txt_fecha_expiracion.Tag & "%" Then fecha_expiracion_param = "%%"
        If numero_plastico_param = "%" & txt_numero_plastico.Tag & "%" Then numero_plastico_param = "%%"
        If numero_chip_param = "%" & txt_numero_chip.Tag & "%" Then numero_chip_param = "%%"
        Me.CedulasTableAdapter1.FillBy(Me.DBDataSet1.cedulas, cedula_param, nombres_param, apellidos_param, nacionalidad_param, sexo_param, lugar_nacimiento_param, fecha_nacimiento_param, profesion_param, telefono_param, direccion_param, fecha_expiracion_param, numero_plastico_param, numero_chip_param)
        Return True
    End Function

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        abrir(ventana_regresar)
    End Sub

    Private Sub Button4_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Derecha.Visible = False
        sync()
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        Derecha.Visible = False
        sync()
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        Derecha.Visible = True
        If SplitContainer1.Panel2Collapsed = False Then
            SplitContainer1.Panel2Collapsed = True
            SplitContainer1.Panel1Collapsed = False
            Imprimir_ventanas.Panel1Collapsed = True
            Imprimir_ventanas.Panel2Collapsed = False
        Else
            Imprimir_ventanas.Panel1Collapsed = True
            Imprimir_ventanas.Panel2Collapsed = False
        End If
    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Application.Exit()
    End Sub

    Private Sub Button5_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Derecha.Visible = True
        If SplitContainer1.Panel2Collapsed = False Then
            SplitContainer1.Panel2Collapsed = True
            SplitContainer1.Panel1Collapsed = False
            Imprimir_ventanas.Panel2Collapsed = True
            Imprimir_ventanas.Panel1Collapsed = False
        Else
            Imprimir_ventanas.Panel2Collapsed = True
            Imprimir_ventanas.Panel1Collapsed = False
        End If
    End Sub

    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'Me.cedulasTableAdapter.Adapter.SelectCommand.CommandText = "Select * from cedulas"
        'Me.cedulasTableAdapter.Fill(Me.CedulasDBDataSet.cedulas)
    End Sub

    Private Sub a1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a1.CheckedChanged
        If a1.Checked = False Then
            DataGridView1.Columns.Item(1).Visible = False
            My.Settings.Tabla_cedula = 0
        Else
            DataGridView1.Columns.Item(1).Visible = True
            My.Settings.Tabla_cedula = 1
        End If
    End Sub

    Private Sub a2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a2.CheckedChanged
        If a2.Checked = False Then
            DataGridView1.Columns.Item(2).Visible = False
            My.Settings.Tabla_nombres = 0
        Else
            DataGridView1.Columns.Item(2).Visible = True
            My.Settings.Tabla_nombres = 1
        End If
    End Sub
    Private Sub a3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a3.CheckedChanged
        If a3.Checked = False Then
            DataGridView1.Columns.Item(3).Visible = False
            My.Settings.Tabla_apellidos = 0
        Else
            DataGridView1.Columns.Item(3).Visible = True
            My.Settings.Tabla_apellidos = 1
        End If
    End Sub
    Private Sub a4_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a4.CheckedChanged
        If a4.Checked = False Then
            DataGridView1.Columns.Item(4).Visible = False
            My.Settings.Tabla_nacionalidad = 0
        Else
            DataGridView1.Columns.Item(4).Visible = True
            My.Settings.Tabla_nacionalidad = 1
        End If
    End Sub
    Private Sub a5_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a5.CheckedChanged
        If a5.Checked = False Then
            DataGridView1.Columns.Item(5).Visible = False
            My.Settings.Tabla_sexo = 0
        Else
            DataGridView1.Columns.Item(5).Visible = True
            My.Settings.Tabla_sexo = 1
        End If
    End Sub
    Private Sub a6_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a6.CheckedChanged
        If a6.Checked = False Then
            DataGridView1.Columns.Item(6).Visible = False
            My.Settings.Tabla_lugar_nacimiento = 0
        Else
            DataGridView1.Columns.Item(6).Visible = True
            My.Settings.Tabla_lugar_nacimiento = 1
        End If
    End Sub
    Private Sub a7_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a7.CheckedChanged
        If a7.Checked = False Then
            DataGridView1.Columns.Item(7).Visible = False
            My.Settings.Tabla_fecha_nacimiento = 0
        Else
            DataGridView1.Columns.Item(7).Visible = True
            My.Settings.Tabla_fecha_nacimiento = 1
        End If
    End Sub
    Private Sub a8_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a8.CheckedChanged
        If a8.Checked = False Then
            DataGridView1.Columns.Item(8).Visible = False
            My.Settings.Tabla_profesion = 0
        Else
            DataGridView1.Columns.Item(8).Visible = True
            My.Settings.Tabla_profesion = 1
        End If
    End Sub
    Private Sub a9_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a9.CheckedChanged
        If a9.Checked = False Then
            DataGridView1.Columns.Item(9).Visible = False
            My.Settings.Tabla_telefono = 0
        Else
            DataGridView1.Columns.Item(9).Visible = True
            My.Settings.Tabla_telefono = 1
        End If
    End Sub
    Private Sub a10_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a10.CheckedChanged
        If a10.Checked = False Then
            DataGridView1.Columns.Item(10).Visible = False
            My.Settings.Tabla_direccion = 0
        Else
            DataGridView1.Columns.Item(10).Visible = True
            My.Settings.Tabla_direccion = 1
        End If
    End Sub
    Private Sub a11_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a11.CheckedChanged
        If a11.Checked = False Then
            DataGridView1.Columns.Item(11).Visible = False
            My.Settings.Tabla_fecha_expiracion = 0
        Else
            DataGridView1.Columns.Item(11).Visible = True
            My.Settings.Tabla_fecha_expiracion = 1
        End If
    End Sub
    Private Sub a12_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a12.CheckedChanged
        If a12.Checked = False Then
            DataGridView1.Columns.Item(12).Visible = False
            My.Settings.Tabla_numero_plastico = 0
        Else
            DataGridView1.Columns.Item(12).Visible = True
            My.Settings.Tabla_numero_plastico = 1
        End If
    End Sub
    Private Sub a13_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a13.CheckedChanged
        If a13.Checked = False Then
            DataGridView1.Columns.Item(13).Visible = False
            My.Settings.Tabla_numero_chip = 0
        Else
            DataGridView1.Columns.Item(13).Visible = True
            My.Settings.Tabla_numero_chip = 1
        End If
    End Sub

    Private Sub a14_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a14.CheckedChanged
        If a14.Checked = False Then
            DataGridView1.Columns.Item(14).Visible = False
            My.Settings.Tabla_email = 0
        Else
            DataGridView1.Columns.Item(14).Visible = True
            My.Settings.Tabla_email = 1
        End If
    End Sub
    Private Sub a15_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a15.CheckedChanged
        If a15.Checked = False Then
            DataGridView1.Columns.Item(15).Visible = False
            My.Settings.Tabla_observaciones = 0
        Else
            DataGridView1.Columns.Item(15).Visible = True
            My.Settings.Tabla_observaciones = 1
        End If
    End Sub
    Private Sub a16_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a16.CheckedChanged
        If a16.Checked = False Then
            DataGridView1.Columns.Item(16).Visible = False
            My.Settings.Tabla_direccion2 = 0
        Else
            DataGridView1.Columns.Item(16).Visible = True
            My.Settings.Tabla_direccion2 = 1
        End If
    End Sub
    Private Sub a17_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a17.CheckedChanged
        If a17.Checked = False Then
            DataGridView1.Columns.Item(17).Visible = False
            My.Settings.Tabla_telefono2 = 0
        Else
            DataGridView1.Columns.Item(17).Visible = True
            My.Settings.Tabla_telefono2 = 1
        End If
    End Sub
    Private Sub a18_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles a18.CheckedChanged
        If a18.Checked = False Then
            DataGridView1.Columns.Item(18).Visible = False
            My.Settings.Tabla_celular2 = 0
        Else
            DataGridView1.Columns.Item(18).Visible = True
            My.Settings.Tabla_celular2 = 1
        End If
    End Sub


    Function tabla_settings()
        If My.Settings.Tabla_cedula = 1 Then a1.Checked = True Else a1.Checked = False
        If My.Settings.Tabla_nombres = 1 Then a2.Checked = True Else a2.Checked = False
        If My.Settings.Tabla_apellidos = 1 Then a3.Checked = True Else a3.Checked = False
        If My.Settings.Tabla_nacionalidad = 1 Then a4.Checked = True Else a4.Checked = False
        If My.Settings.Tabla_sexo = 1 Then a5.Checked = True Else a5.Checked = False
        If My.Settings.Tabla_lugar_nacimiento = 1 Then a6.Checked = True Else a6.Checked = False
        If My.Settings.Tabla_fecha_nacimiento = 1 Then a7.Checked = True Else a7.Checked = False
        If My.Settings.Tabla_profesion = 1 Then a8.Checked = True Else a8.Checked = False
        If My.Settings.Tabla_telefono = 1 Then a9.Checked = True Else a9.Checked = False
        If My.Settings.Tabla_direccion = 1 Then a10.Checked = True Else a10.Checked = False
        If My.Settings.Tabla_fecha_expiracion = 1 Then a11.Checked = True Else a11.Checked = False
        If My.Settings.Tabla_numero_plastico = 1 Then a12.Checked = True Else a12.Checked = False
        If My.Settings.Tabla_numero_chip = 1 Then a13.Checked = True Else a13.Checked = False
        If My.Settings.Tabla_email = 1 Then a14.Checked = True Else a14.Checked = False
        If My.Settings.Tabla_observaciones = 1 Then a15.Checked = True Else a15.Checked = False
        If My.Settings.Tabla_direccion2 = 1 Then a16.Checked = True Else a16.Checked = False
        If My.Settings.Tabla_telefono2 = 1 Then a17.Checked = True Else a17.Checked = False
        If My.Settings.Tabla_celular2 = 1 Then a18.Checked = True Else a18.Checked = False

        DataGridView1.Columns.Item(1).Width = My.Settings.Tabla_cedula_ancho
        DataGridView1.Columns.Item(2).Width = My.Settings.Tabla_nombres_ancho
        DataGridView1.Columns.Item(3).Width = My.Settings.Tabla_apellidos_ancho
        DataGridView1.Columns.Item(4).Width = My.Settings.Tabla_nacionalidad_ancho
        DataGridView1.Columns.Item(5).Width = My.Settings.Tabla_sexo_ancho
        DataGridView1.Columns.Item(6).Width = My.Settings.Tabla_lugar_nacimiento_ancho
        DataGridView1.Columns.Item(7).Width = My.Settings.Tabla_fecha_nacimiento_ancho
        DataGridView1.Columns.Item(8).Width = My.Settings.Tabla_profesion_ancho
        DataGridView1.Columns.Item(9).Width = My.Settings.Tabla_telefono_ancho
        DataGridView1.Columns.Item(10).Width = My.Settings.Tabla_direccion_ancho
        DataGridView1.Columns.Item(11).Width = My.Settings.Tabla_fecha_expiracion_ancho
        DataGridView1.Columns.Item(12).Width = My.Settings.Tabla_numero_plastico_ancho
        DataGridView1.Columns.Item(13).Width = My.Settings.Tabla_numero_chip_ancho
        DataGridView1.Columns.Item(14).Width = My.Settings.Tabla_email_ancho
        DataGridView1.Columns.Item(15).Width = My.Settings.Tabla_observaciones_ancho
        DataGridView1.Columns.Item(16).Width = My.Settings.Tabla_direccion2_ancho
        DataGridView1.Columns.Item(17).Width = My.Settings.Tabla_telefono2_ancho
        DataGridView1.Columns.Item(18).Width = My.Settings.Tabla_celular2_ancho


        tableiniatilize = True
        Return True
    End Function

    Dim id_reporte_interno As String

    Private Sub DataGridView1_CellContentClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellContentClick
        If e.ColumnIndex = 0 Then
            Dim i As Integer = Me.DataGridView1.CurrentRow.Index
            Dim dato As String = Me.DataGridView1.Item(0, i).Value
            id_reporte_interno = dato
            Me.CedulasTableAdapter.Adapter.SelectCommand.CommandText = "Select * from cedulas where id='" + dato + "'"
            Me.CedulasTableAdapter.Fill(Me.DBDataSet2.cedulas)
            Me.ReportViewer2.RefreshReport()
            abrir("Reporte_interno")
        End If
    End Sub

    Private Sub DataGridView1_ColumnWidthChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewColumnEventArgs) Handles DataGridView1.ColumnWidthChanged
        If tableiniatilize = True Then
            If e.Column.Index = 1 Then My.Settings.Tabla_cedula_ancho = e.Column.Width
            If e.Column.Index = 2 Then My.Settings.Tabla_nombres_ancho = e.Column.Width
            If e.Column.Index = 3 Then My.Settings.Tabla_apellidos_ancho = e.Column.Width
            If e.Column.Index = 4 Then My.Settings.Tabla_nacionalidad_ancho = e.Column.Width
            If e.Column.Index = 5 Then My.Settings.Tabla_sexo_ancho = e.Column.Width
            If e.Column.Index = 6 Then My.Settings.Tabla_lugar_nacimiento_ancho = e.Column.Width
            If e.Column.Index = 7 Then My.Settings.Tabla_fecha_nacimiento_ancho = e.Column.Width
            If e.Column.Index = 8 Then My.Settings.Tabla_profesion_ancho = e.Column.Width
            If e.Column.Index = 9 Then My.Settings.Tabla_telefono_ancho = e.Column.Width
            If e.Column.Index = 10 Then My.Settings.Tabla_direccion_ancho = e.Column.Width
            If e.Column.Index = 11 Then My.Settings.Tabla_fecha_expiracion_ancho = e.Column.Width
            If e.Column.Index = 12 Then My.Settings.Tabla_numero_plastico_ancho = e.Column.Width
            If e.Column.Index = 13 Then My.Settings.Tabla_numero_chip_ancho = e.Column.Width
            If e.Column.Index = 14 Then My.Settings.Tabla_email_ancho = e.Column.Width
            If e.Column.Index = 15 Then My.Settings.Tabla_observaciones_ancho = e.Column.Width
            If e.Column.Index = 16 Then My.Settings.Tabla_direccion2_ancho = e.Column.Width
            If e.Column.Index = 17 Then My.Settings.Tabla_telefono2_ancho = e.Column.Width
            If e.Column.Index = 18 Then My.Settings.Tabla_celular2_ancho = e.Column.Width

        End If
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If SetupThePrinting() Then PrintDocument1.Print()
    End Sub

    Private Sub PrintDocument1_PrintPage(ByVal sender As Object, ByVal e As System.Drawing.Printing.PrintPageEventArgs) Handles PrintDocument1.PrintPage
        Dim more As Boolean
        Try
            more = MyDataGridViewPrinter.DrawDataGridView(e.Graphics)
            If more Then e.HasMorePages = True
        Catch Ex As Exception
            MessageBox.Show(Ex.Message & vbCrLf & Ex.StackTrace, "prueba", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Function SetupThePrinting() As Boolean
        Dim MyPrintDialog As PrintDialog = New PrintDialog()
        MyPrintDialog.AllowCurrentPage = False
        MyPrintDialog.AllowPrintToFile = False
        MyPrintDialog.AllowSelection = False
        MyPrintDialog.AllowSomePages = True
        MyPrintDialog.PrintToFile = False
        MyPrintDialog.ShowHelp = False
        MyPrintDialog.ShowNetwork = False
        MyPrintDialog.PrinterSettings.DefaultPageSettings.Landscape = True
        If MyPrintDialog.ShowDialog() <> System.Windows.Forms.DialogResult.OK Then Return False
        PrintDocument1.DocumentName = "REPORTE / Listado"
        PrintDocument1.PrinterSettings = MyPrintDialog.PrinterSettings
        PrintDocument1.DefaultPageSettings = MyPrintDialog.PrinterSettings.DefaultPageSettings
        PrintDocument1.DefaultPageSettings.Margins = New Margins(40, 40, 40, 40)
        If MessageBox.Show("Do you want the report to be centered on the page", "InvoiceManager - Center on Page", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then
            MyDataGridViewPrinter = New DataGridViewPrinter(DataGridView1, PrintDocument1, True, True, "REPORTE / Listado", New Font("Tahoma", 18, FontStyle.Bold, GraphicsUnit.Point), Color.Black, True)
        Else
            MyDataGridViewPrinter = New DataGridViewPrinter(DataGridView1, PrintDocument1, False, True, "REPORTE / Listado", New Font("Tahoma", 18, FontStyle.Bold, GraphicsUnit.Point), Color.Black, True)
        End If
        Return True
    End Function

    Private Sub Button11_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        If SetupThePrinting() Then
            Dim MyPrintPreviewDialog As PrintPreviewDialog = New PrintPreviewDialog()
            MyPrintPreviewDialog.Document = PrintDocument1
            MyPrintPreviewDialog.ShowDialog()
        End If
    End Sub

    Private Sub Button8_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        sysRecientes()
    End Sub

    Private Sub Button15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button15.Click
        abrir("imprimir", Ventana_redirectGlobal)
    End Sub

    Private Sub Panel8_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel8.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub FlowLayoutPanel2_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles FlowLayoutPanel2.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub Panel17_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel17.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub Panel1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel1.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub Panel19_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel19.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub fail_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles fail.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub Error_1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Error_1.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub Report_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Report.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            ' pasar el formulario como parámetro  
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub id_file_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles id_file.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            Mover_Formulario(Me)
        End If
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        Application.Exit()
    End Sub

    Private Sub Button1_Click_6(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Me.WindowState = FormWindowState.Maximized
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
        Button1.Visible = False
        Button9.Visible = True
    End Sub

    Private Sub Button2_Click_3(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Me.WindowState = FormWindowState.Minimized
    End Sub

    Private Sub Button9_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        Me.WindowState = FormWindowState.Normal
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
        Button1.Visible = True
        Button9.Visible = False
    End Sub

    Private Sub PictureBox2_Layout(ByVal sender As Object, ByVal e As System.Windows.Forms.LayoutEventArgs) Handles PictureBox2.Layout

    End Sub

    Private Sub PictureBox2_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox2.MouseDown
        Me.Cursor = Cursors.SizeNWSE
        recisetimer.Start()
        sync()
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
    End Sub

    Function sysRecientes()
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Dim regis, nombre, cedula, fecha As String
        Dim objReader As SQLiteDataReader
        Try
            objConn = New SQLiteConnection(CONNECTION_STR)
            objConn.Open()
            objCommand = objConn.CreateCommand()
            If (search_del.Text = "0" And search_al.Text= "0") Then
                objCommand.CommandText = "SELECT * FROM cedulas ORDER BY id DESC limit 0,25"
            Else
                objCommand.CommandText = "SELECT * FROM cedulas ORDER BY id ASC limit " + search_del.Text + "," + search_al.Text + ""
            End If


            objReader = objCommand.ExecuteReader()
            FlowLayoutPanel1.Controls.Clear()
            While (objReader.Read())
                If Not IsDBNull(objReader("foto")) Then


                    If IsDBNull(objReader("nombres")) Then
                        nombre = "No se asigno nombre"
                    Else
                        nombre = objReader("nombres")
                    End If

                    If IsDBNull(objReader("cedula")) Then
                        cedula = "0000000000"
                    Else
                        cedula = objReader("cedula")
                    End If

                    If IsDBNull(objReader("fecha_modificado")) Then
                        fecha = objReader("fecha_save")
                    Else
                        fecha = objReader("fecha_modificado")
                    End If


                    If (nombre = "") Then
                        nombre = "Sin nombre asignado"
                    End If
                    If (cedula = "") Then
                        cedula = "0000000000"
                    End If
                    regis = objReader("id")
                    crearitem(regis, nombre, objReader("foto"), cedula, fecha)
                End If
            End While
        Catch ex As Exception
            MessageBox.Show("An error has occurred: " & ex.Message)
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try
        Return True
    End Function

    Function delete_Record(ByRef id As String)
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Dim objReader As SQLiteDataReader
        Try
            objConn = New SQLiteConnection(CONNECTION_STR)
            objConn.Open()
            objCommand = objConn.CreateCommand()
            objCommand.CommandText = "DELETE FROM cedulas WHERE id ='" + id + "'"
            objReader = objCommand.ExecuteReader()
            abrir("imprimir")
        Catch ex As Exception
            MessageBox.Show("An error has occurred: " & ex.Message)
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try
        Return True
    End Function

    Function crearitem(ByRef registro As String, ByRef nombre As String, ByRef foto As Byte(), ByRef cedula As String, ByRef fecha As String)
        Dim recientespanel As Panel = New Panel()
        Dim recientesfoto As PictureBox = New PictureBox()
        Dim recientesnombre As Label = New Label()
        Dim recientesregistro As Label = New Label()
        Dim recientescedula As Label = New Label()
        Dim recientesfecha As Label = New Label()
        recientesnombre.Text = nombre
        recientescedula.Text = cedula
        recientesregistro.Text = registro + " |"
        recientesfecha.Text = fecha
        recientespanel.Width = 206
        recientespanel.Height = 65
        recientesfoto.Width = 45
        recientesfoto.Height = 55
        If Not foto Is Nothing Then
            Using ms = New MemoryStream(foto, 0, foto.Length)
                ms.Write(foto, 0, foto.Length)
                recientesfoto.Image = Image.FromStream(ms, True)
            End Using
        End If
        recientesfoto.SizeMode = 1
        recientesfoto.Top = 6
        recientesfoto.Left = 18
        recientesfoto.Tag = registro
        recientesnombre.Top = 4
        recientesnombre.Left = 65
        recientesnombre.Width = 140
        recientesnombre.Height = 17

        recientesregistro.Top = 24
        recientesregistro.Left = 65
        recientesregistro.Width = 55
        recientesregistro.Height = 17
        Dim recientesregistrofont As Font = New Font("Arial", 11.25, FontStyle.Regular)
        Dim recientesnombrefont As Font = New Font("Segoe UI", 9.75, FontStyle.Bold)
        Dim recientescedulafont As Font = New Font("Segoe UI", 11.25, FontStyle.Regular)
        Dim recientesfechafont As Font = New Font("Segoe UI", 8.25, FontStyle.Regular)
        recientesregistro.Font = recientesregistrofont
        recientesregistro.ForeColor = Color.DimGray
        recientesnombre.Font = recientesnombrefont
        recientescedula.Top = 22
        recientescedula.Left = 115
        recientescedula.Width = 128
        recientescedula.Height = 17
        recientescedula.Font = recientescedulafont
        recientescedula.ForeColor = Color.DimGray
        recientesfecha.Top = 44
        recientesfecha.Left = 65
        recientesfecha.Width = 128
        recientesfecha.Height = 17
        recientesfecha.Font = recientesfechafont
        recientesfecha.ForeColor = Color.DimGray
        AddHandler recientesfoto.Click, AddressOf PrintMessage
        recientespanel.Controls.Add(recientesfoto)
        recientespanel.Controls.Add(recientesregistro)
        recientespanel.Controls.Add(recientesnombre)
        recientespanel.Controls.Add(recientescedula)
        recientespanel.Controls.Add(recientesfecha)
        FlowLayoutPanel1.Controls.Add(recientespanel)
        Return True
    End Function
    Private Sub PrintMessage(ByVal sender As System.Object, ByVal e As System.EventArgs)
        clear()
        id_reporte_interno = sender.tag.ToString
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Dim objReader As SQLiteDataReader
        Try
            objConn = New SQLiteConnection(CONNECTION_STR)
            objConn.Open()
            objCommand = objConn.CreateCommand()
            objCommand.CommandText = "SELECT * FROM cedulas WHERE id='" + sender.tag.ToString + "'"
            objReader = objCommand.ExecuteReader()
            While (objReader.Read())
                If Not objReader("numero_chip") = "" Then

                End If

                Me.d_cedula.Text = objReader("cedula")
                Me.d_nombres.Text = objReader("nombres")
                Me.d_apellidos.Text = objReader("apellidos")
                Me.d_nacionalidad.Text = objReader("nacionalidad")
                Me.d_fecha_nac.Text = objReader("fecha_nacimiento")
                Me.d_sexo.Text = objReader("sexo")
                Me.d_lugar_nac.Text = objReader("lugar_nacimiento")
                Me.d_telefono.Text = objReader("telefono")
                Me.d_profesion.Text = objReader("profesion")
                Me.d_direccion.Text = objReader("direccion")
                Me.d_numero_chip.Text = objReader("numero_chip")
                Me.d_numero_plastico.Text = objReader("numero_plastico")
                Me.d_fecha_expiracion.Text = objReader("fecha_expiracion")
                Me.campo1.Text = objReader("observaciones")
                Me.campo2.Text = objReader("email")
                Me.campo3.Text = CStr(objReader("direccion2"))
                Me.campo4.Text = CStr(objReader("telefono2"))
                Me.campo5.Text = CStr(objReader("celular2"))

                If Not Me.d_numero_chip.Text = "" Then
                    Me.d_nombres.Enabled = False
                    Me.d_apellidos.Enabled = False
                    Me.d_nacionalidad.Enabled = False
                    Me.d_fecha_nac.Enabled = False
                    Me.d_sexo.Enabled = False
                    Me.d_numero_plastico.Enabled = False
                    Me.d_cedula.Enabled = False
                    Me.d_fecha_expiracion.Enabled = False
                    Me.d_lugar_nac.Enabled = False
                    Me.d_telefono.Enabled = False
                    Me.d_profesion.Enabled = False
                    Me.d_direccion.Enabled = False
                    Me.d_numero_chip.Enabled = False
                End If
                Me.lbl_id.Text = objReader("id")
                If Not IsDBNull(objReader("foto")) Then
                    photo = objReader("foto")
                    Using ms = New MemoryStream(objReader("foto"), 0, objReader("foto").Length)
                        ms.Write(objReader("foto"), 0, objReader("foto").Length)
                        d_foto.Image = Image.FromStream(ms, True)
                    End Using
                End If
                'If Not IsDBNull(objReader("firma")) Then
                'signature = objReader("firma")
                ' Using ms = New MemoryStream(objReader("firma"), 0, objReader("firma").Length)
                '    ms.Write(objReader("firma"), 0, objReader("firma").Length)
                '   d_firma.Image = Image.FromStream(ms, True)
                ' End Using
                'End If
                ' If Not IsDBNull(objReader("huella") ) Then
                ' fingerIcao = objReader("huella")
                'Using ms = New MemoryStream(objReader("huella"), 0, objReader("huella").Length)
                'MS.Write(objReader("huella"), 0, objReader("huella").Length)
                'd_huella.Image = Image.FromStream(MS, True)
                ' End Using
                'End If
                ' If Not IsDBNull(objReader("huellasensor")) Then
                ' Using ms = New MemoryStream(objReader("huellasensor"), 0, objReader("huellasensor").Length)
                'MS.Write(objReader("huellasensor"), 0, objReader("huellasensor").Length)
                'd_sensor.Image = Image.FromStream(MS, True)
                'End Using
                'End If
                If Not IsDBNull(objReader("ladoA")) Then
                    Using ms = New MemoryStream(objReader("ladoA"), 0, objReader("ladoA").Length)
                        ms.Write(objReader("ladoA"), 0, objReader("ladoA").Length)
                        LADOA.Image = Image.FromStream(ms, True)
                    End Using
                End If
                If Not IsDBNull(objReader("ladoB")) Then
                    Using ms = New MemoryStream(objReader("ladoB"), 0, objReader("ladoB").Length)
                        ms.Write(objReader("ladoB"), 0, objReader("ladoB").Length)
                        LADOB.Image = Image.FromStream(ms, True)
                    End Using
                End If
                ' v_6.Text = objReader("fecha_comparacion")
            End While
            abrir("Datos")
            btn_4.Text = "GUARDAR CAMBIOS"
        Catch ex As Exception
            MessageBox.Show("An error has occurred: " & ex.Message)
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try



    End Sub
    Private Sub PictureBox2_MouseHover(ByVal sender As Object, ByVal e As System.EventArgs) Handles PictureBox2.MouseHover
        Me.Cursor = Cursors.SizeNWSE
    End Sub

    Private Sub PictureBox2_MouseLeave(ByVal sender As Object, ByVal e As System.EventArgs) Handles PictureBox2.MouseLeave
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub PictureBox2_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox2.MouseMove
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
    End Sub

    Private Sub PictureBox2_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox2.MouseUp
        Me.Cursor = Cursors.Default
        recisetimer.Stop()
        sync()
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
    End Sub
    Dim clx, alx, cly, aly As Integer

    Private Sub sync()
        alx = Me.Location.X
        clx = Cursor.Position.X
        aly = Me.Location.Y
        cly = Cursor.Position.Y
    End Sub

    Private Sub recisetimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles recisetimer.Tick
        Me.Width = (clx + 10) - alx
        Me.Height = (cly + 10) - aly
        sync()
    End Sub

    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        delete_Record(id_reporte_interno)
        sysRecientes()
    End Sub

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
      
    End Sub

    Private Function clear()
        idsys()
        leido = False
        Me.zoombox.Image = Nothing
        Me.d_nombres.Enabled = True
        Me.d_apellidos.Enabled = True
        Me.d_nacionalidad.Enabled = True
        Me.d_fecha_nac.Enabled = True
        Me.d_sexo.Enabled = True
        Me.d_cedula.Enabled = True
        Me.d_fecha_expiracion.Enabled = True
        Me.d_lugar_nac.Enabled = True
        Me.d_telefono.Enabled = True
        Me.d_profesion.Enabled = True
        Me.d_direccion.Enabled = True
        Me.campo1.Text = ""
        Me.campo2.Text = ""
        Me.campo3.Text = ""
        Me.campo4.Text = ""
        Me.campo5.Text = ""
        Me.d_nombres.Text = ""
        Me.d_apellidos.Text = ""
        Me.d_nacionalidad.Text = ""
        Me.d_fecha_nac.Text = ""
        Me.d_sexo.Text = ""
        Me.d_numero_plastico.Text = ""
        Me.d_cedula.Text = ""
        Me.d_fecha_expiracion.Text = ""
        Me.d_lugar_nac.Text = ""
        Me.d_telefono.Text = ""
        Me.d_profesion.Text = ""
        Me.d_direccion.Text = ""
        Me.d_numero_chip.Text = ""
        Me.d_firma.Image = Nothing
        Me.d_foto.Image = Netcell_Demo_v._4.My.Resources.Resources.man
        Me.d_foto.Refresh()
        Me.LADOA.Image = Nothing
        Me.LADOB.Image = Nothing
        Me.d_huella.Image = Nothing
        Me.d_sensor.Image = Nothing
        certificado.Visible = False
        certificado_txt.Visible = False
        d_dedo.Image = Netcell_Demo_v._4.My.Resources.Resources.d0
        d_mano.Text = "Mano Derecha"
        Me.v_1.Visible = False
        Me.v_2.Visible = False
        Me.v_3.Visible = False
        Me.v_6.Visible = False
        Me.v_sello.Visible = False
        Me.lbl_footer_1.Text = "0-10 Segundos"
        Me.lbl_footer_2.Text = "TIEMPO APROXIMADO DE LECTURA"
        messageText = Nothing

        firstName = Nothing
        lastName = Nothing
        dateOfBirth = Nothing
        validUntil = Nothing
        nationality = Nothing
        sex = Nothing
        documentNumber = Nothing
        firstFingerIcaoIndex = Nothing
        secondFingerIcaoIndex = Nothing
        firstFingerTemplate = Nothing
        secondFingerTemplate = Nothing
        photo = Nothing
        fingerIcao = Nothing
        signature = Nothing
        chipId = Nothing
        dateOfBirth_11 = Nothing
        personalID_11 = Nothing
        checkDigit1 = Nothing
        checkDigit2 = Nothing
        checkDigit3 = Nothing
        checkDigit4 = Nothing
        documentCode = Nothing
        issuingState = Nothing
        holderName = Nothing
        holderName_11 = Nothing
        placeOfBirth = Nothing
        telephone = Nothing
        profession = Nothing
        address = Nothing
        Return True
    End Function

    Private Sub btn_3_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_3.Click
        clear()
        btn_4.Text = "GUARDAR"
    End Sub

    Private Sub Button8_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        clear()
        Dim objConn As SQLiteConnection = Nothing
        Dim objCommand As SQLiteCommand
        Dim objReader As SQLiteDataReader
        Try
            objConn = New SQLiteConnection(CONNECTION_STR)
            objConn.Open()
            objCommand = objConn.CreateCommand()
            objCommand.CommandText = "SELECT * FROM cedulas WHERE id='" + id_reporte_interno + "'"
            objReader = objCommand.ExecuteReader()
            While (objReader.Read())

            
                Me.d_cedula.Text = objReader("cedula")
                Me.d_nombres.Text = objReader("nombres")
                Me.d_apellidos.Text = objReader("apellidos")
                Me.d_nacionalidad.Text = objReader("nacionalidad")
                Me.d_fecha_nac.Text = objReader("fecha_nacimiento")
                Me.d_sexo.Text = objReader("sexo")
                Me.d_lugar_nac.Text = objReader("lugar_nacimiento")
                Me.d_telefono.Text = objReader("telefono")
                Me.d_profesion.Text = objReader("profesion")
                Me.d_direccion.Text = objReader("direccion")
                Me.d_numero_chip.Text = objReader("numero_chip")
                Me.d_numero_plastico.Text = objReader("numero_plastico")
                Me.d_fecha_expiracion.Text = objReader("fecha_expiracion")
                Me.campo1.Text = objReader("observaciones")
                Me.campo2.Text = objReader("email")
                Me.campo3.Text = CStr(objReader("direccion2"))
                Me.campo4.Text = CStr(objReader("telefono2"))
                Me.campo5.Text = CStr(objReader("celular2"))
                If Not Me.d_numero_chip.Text = "" Then
                    Me.d_nombres.Enabled = False
                    Me.d_apellidos.Enabled = False
                    Me.d_nacionalidad.Enabled = False
                    Me.d_fecha_nac.Enabled = False
                    Me.d_sexo.Enabled = False
                    Me.d_numero_plastico.Enabled = False
                    Me.d_cedula.Enabled = False
                    Me.d_fecha_expiracion.Enabled = False
                    Me.d_lugar_nac.Enabled = False
                    Me.d_telefono.Enabled = False
                    Me.d_profesion.Enabled = False
                    Me.d_direccion.Enabled = False
                    Me.d_numero_chip.Enabled = False
                End If
                Me.lbl_id.Text = objReader("id")
                If Not IsDBNull(objReader("foto")) Then
                    photo = objReader("foto")
                    Using ms = New MemoryStream(objReader("foto"), 0, objReader("foto").Length)
                        ms.Write(objReader("foto"), 0, objReader("foto").Length)
                        d_foto.Image = Image.FromStream(ms, True)
                    End Using
                End If
                'If Not IsDBNull(objReader("firma")) Then
                'signature = objReader("firma")
                ' Using ms = New MemoryStream(objReader("firma"), 0, objReader("firma").Length)
                '    ms.Write(objReader("firma"), 0, objReader("firma").Length)
                '   d_firma.Image = Image.FromStream(ms, True)
                ' End Using
                'End If
                ' If Not IsDBNull(objReader("huella") ) Then
                ' fingerIcao = objReader("huella")
                'Using ms = New MemoryStream(objReader("huella"), 0, objReader("huella").Length)
                'MS.Write(objReader("huella"), 0, objReader("huella").Length)
                'd_huella.Image = Image.FromStream(MS, True)
                ' End Using
                'End If
                ' If Not IsDBNull(objReader("huellasensor")) Then
                ' Using ms = New MemoryStream(objReader("huellasensor"), 0, objReader("huellasensor").Length)
                'MS.Write(objReader("huellasensor"), 0, objReader("huellasensor").Length)
                'd_sensor.Image = Image.FromStream(MS, True)
                'End Using
                'End If
                If Not IsDBNull(objReader("ladoA")) Then
                    Using ms = New MemoryStream(objReader("ladoA"), 0, objReader("ladoA").Length)
                        ms.Write(objReader("ladoA"), 0, objReader("ladoA").Length)
                        LADOA.Image = Image.FromStream(ms, True)
                    End Using
                End If
                If Not IsDBNull(objReader("ladoB")) Then
                    Using ms = New MemoryStream(objReader("ladoB"), 0, objReader("ladoB").Length)
                        ms.Write(objReader("ladoB"), 0, objReader("ladoB").Length)
                        LADOB.Image = Image.FromStream(ms, True)
                    End Using
                End If
                ' v_6.Text = objReader("fecha_comparacion")
            End While
            abrir("Datos")
            btn_4.Text = "GUARDAR CAMBIOS"
        Catch ex As Exception
            MessageBox.Show("An error has occurred: " & ex.Message)
        Finally
            If Not IsNothing(objConn) Then
                objConn.Close()
            End If
        End Try
    End Sub

    Private Sub LADOA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOA.Click
        If (e.Button = Windows.Forms.MouseButtons.Right) Then
            TrackBar1.Focus()
            zoom()
        End If
        If (e.Button = Windows.Forms.MouseButtons.Middle) Then
            If opcion3 = True Then
                opcion3 = False
            Else
                opcion3 = True
            End If
            TrackBar1.Focus()
            zoom()
        End If
    End Sub

    Private Sub LADOA_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOA.MouseDown
        Try
            If e.Button = MouseButtons.Left Then
                cropX = e.X
                cropY = e.Y
                cropPen = New Pen(cropPenColor, cropPenSize)
                cropPen.DashStyle = cropDashStyle
                Cursor = Cursors.Cross
            End If
            LADOA.Refresh()
        Catch exc As Exception
        End Try
    End Sub

    Private Sub LADOA_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOA.MouseMove
        Try
            If LADOA.Image Is Nothing Then Exit Sub
            If e.Button = MouseButtons.Left Then
                LADOA.Refresh()
                cropWidth = e.X - cropX
                cropHeight = e.Y - cropY
                LADOA.CreateGraphics.DrawRectangle(cropPen, cropX, cropY, cropWidth, cropHeight)
            End If
            GC.Collect()
        Catch exc As Exception
            If Err.Number = 5 Then Exit Sub
        End Try
    End Sub

    Private Sub LADOA_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOA.MouseUp

        Try
            Cursor = Cursors.Default
            Try
                If e.Button = MouseButtons.Left Then
                    If leido = False Then
                        If cropWidth < 1 Then
                            Exit Sub
                        End If
                        Dim rect As Rectangle = New Rectangle(cropX, cropY, cropWidth, cropHeight)
                        'First we define a rectangle with the help of already calculated points
                        Dim OriginalImage As Bitmap = New Bitmap(LADOA.Image, LADOA.Width, LADOA.Height)
                        'Original image
                        Dim _img As New Bitmap(cropWidth, cropHeight) ' for cropinf image
                        Dim g As Graphics = Graphics.FromImage(_img) ' create graphics
                        g.InterpolationMode = Drawing2D.InterpolationMode.HighQualityBicubic
                        g.PixelOffsetMode = Drawing2D.PixelOffsetMode.HighQuality
                        g.CompositingQuality = Drawing2D.CompositingQuality.HighQuality
                        'set image attributes
                        g.DrawImage(OriginalImage, 0, 0, rect, GraphicsUnit.Pixel)

                        d_foto.Image = _img
                    End If
                End If
            Catch ex As Exception
            End Try
        Catch ex As Exception
        End Try

    End Sub

    Private Sub TrackBar1_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBar1.Scroll
        zoom()
    End Sub

    Function zoom()
        Dim ed As Graphics = Me.zoombox.CreateGraphics
        Dim hr As Integer = Screen.PrimaryScreen.Bounds.Width
        Dim vr As Integer = Screen.PrimaryScreen.Bounds.Height
        Dim percent As Single = (100 - TrackBar1.Value) / 100
        Dim lengthX As Single = (Me.zoombox.Width) * percent
        Dim lengthY As Single = (Me.zoombox.Height) * percent
        Dim offsetX As Single = lengthX \ 2
        Dim offsetY As Single = lengthY \ 2
        Dim blitAreaX As Integer = Me.zoombox.Width
        Dim blitAreaY As Integer = Me.zoombox.Height
        Dim b As New Bitmap(CInt(blitAreaX), CInt(blitAreaY))
        Dim g As Graphics = Graphics.FromImage(b)
        g.SmoothingMode = Drawing2D.SmoothingMode.HighSpeed
        g.CompositingQuality = Drawing2D.CompositingQuality.HighSpeed
        g.PixelOffsetMode = Drawing2D.PixelOffsetMode.HighSpeed
        g.InterpolationMode = Drawing2D.InterpolationMode.Low
        ed.SmoothingMode = Drawing2D.SmoothingMode.HighSpeed
        ed.CompositingQuality = Drawing2D.CompositingQuality.HighSpeed
        ed.PixelOffsetMode = Drawing2D.PixelOffsetMode.HighSpeed
        ed.InterpolationMode = Drawing2D.InterpolationMode.Low
        Dim hWndWindow As IntPtr = GetDesktopWindow()
        Dim hdcWindow As IntPtr = GetDC(hWndWindow)
        Dim hdcGraphics As IntPtr = g.GetHdc()
        BitBlt(hdcGraphics.ToInt32, 0, 0, blitAreaX, blitAreaY, hdcWindow.ToInt32, Cursor.Current.Position.X - offsetX, Cursor.Current.Position.Y - offsetY, SRCCOPY Or CAPTUREBLT Or NOMIRRORBITMAP)
        If opcion3 = True Then
            BitBlt(hdcGraphics.ToInt32, 0, 0, blitAreaX, blitAreaY, hdcGraphics.ToInt32, 0, 0, NOTSRCCOPY)
        End If
        opcion1 = True
        ReleaseDC(hWndWindow.ToInt32, hdcWindow.ToInt32)
        g.ReleaseHdc(hdcGraphics)
        If opcion1 = True Or opcion3 = True Then
            ed.DrawImage(b, New Rectangle(0, 0, blitAreaX, blitAreaY), 0, 0, lengthX, lengthY, GraphicsUnit.Pixel)
        End If
        ed.Dispose()
    End Function


    Function zoomClear()
        Dim ed As Graphics = Me.zoombox.CreateGraphics
        Dim hr As Integer = Screen.PrimaryScreen.Bounds.Width
        Dim vr As Integer = Screen.PrimaryScreen.Bounds.Height
        Dim percent As Single = 0
        Dim lengthX As Single = 0
        Dim lengthY As Single = 0
        Dim offsetX As Single = 0
        Dim offsetY As Single = 0
        Dim blitAreaX As Integer = 1
        Dim blitAreaY As Integer = 1
        Dim b As New Bitmap(CInt(blitAreaX), CInt(blitAreaY))
        Dim g As Graphics = Graphics.FromImage(b)
        g.SmoothingMode = Drawing2D.SmoothingMode.HighSpeed
        g.CompositingQuality = Drawing2D.CompositingQuality.HighSpeed
        g.PixelOffsetMode = Drawing2D.PixelOffsetMode.HighSpeed
        g.InterpolationMode = Drawing2D.InterpolationMode.Low
        ed.SmoothingMode = Drawing2D.SmoothingMode.HighSpeed
        ed.CompositingQuality = Drawing2D.CompositingQuality.HighSpeed
        ed.PixelOffsetMode = Drawing2D.PixelOffsetMode.HighSpeed
        ed.InterpolationMode = Drawing2D.InterpolationMode.Low
        Dim hWndWindow As IntPtr = GetDesktopWindow()
        Dim hdcWindow As IntPtr = GetDC(hWndWindow)
        Dim hdcGraphics As IntPtr = g.GetHdc()
        BitBlt(hdcGraphics.ToInt32, 0, 0, blitAreaX, blitAreaY, hdcWindow.ToInt32, Cursor.Current.Position.X - offsetX, Cursor.Current.Position.Y - offsetY, SRCCOPY Or CAPTUREBLT Or NOMIRRORBITMAP)
        If opcion3 = True Then
            BitBlt(hdcGraphics.ToInt32, 0, 0, blitAreaX, blitAreaY, hdcGraphics.ToInt32, 0, 0, NOTSRCCOPY)
        End If
        ReleaseDC(hWndWindow.ToInt32, hdcWindow.ToInt32)
        g.ReleaseHdc(hdcGraphics)
        If opcion1 = True Or opcion3 = True Then
            ed.DrawImage(b, New Rectangle(0, 0, blitAreaX, blitAreaY), 0, 0, lengthX, lengthY, GraphicsUnit.Pixel)
        End If
        ed.Dispose()
    End Function

    Private Sub LADOB_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOB.Click
        If (e.Button = Windows.Forms.MouseButtons.Right) Then
            TrackBar1.Focus()
            zoom()
        End If
        If (e.Button = Windows.Forms.MouseButtons.Middle) Then
            If opcion3 = True Then
                opcion3 = False
            Else
                opcion3 = True
            End If
            TrackBar1.Focus()
            zoom()
        End If
    End Sub

    Private Sub LADOB_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOB.MouseDown
        Try
            If e.Button = MouseButtons.Left Then
                cropX = e.X
                cropY = e.Y
                cropPen = New Pen(cropPenColor, cropPenSize)
                cropPen.DashStyle = cropDashStyle
                Cursor = Cursors.Cross
            End If
            LADOB.Refresh()
        Catch exc As Exception
        End Try
    End Sub

    Private Sub LADOB_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOB.MouseMove
        Try
            If LADOB.Image Is Nothing Then Exit Sub
            If e.Button = MouseButtons.Left Then
                LADOB.Refresh()
                cropWidth = e.X - cropX
                cropHeight = e.Y - cropY
                LADOB.CreateGraphics.DrawRectangle(cropPen, cropX, cropY, cropWidth, cropHeight)
            End If
            GC.Collect()
        Catch exc As Exception
            If Err.Number = 5 Then Exit Sub
        End Try
    End Sub

    Private Sub LADOB_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LADOB.MouseUp
        Cursor = Cursors.Default
        Try
            If e.Button = MouseButtons.Left Then
                If leido = False Then
                    If cropWidth < 1 Then
                        Exit Sub
                    End If
                    Dim rect As Rectangle = New Rectangle(cropX, cropY, cropWidth, cropHeight)
                    'First we define a rectangle with the help of already calculated points
                    Dim OriginalImage As Bitmap = New Bitmap(LADOB.Image, LADOB.Width, LADOB.Height)
                    'Original image
                    Dim _img As New Bitmap(cropWidth, cropHeight) ' for cropinf image
                    Dim g As Graphics = Graphics.FromImage(_img) ' create graphics
                    g.InterpolationMode = Drawing2D.InterpolationMode.HighQualityBicubic
                    g.PixelOffsetMode = Drawing2D.PixelOffsetMode.HighQuality
                    g.CompositingQuality = Drawing2D.CompositingQuality.HighQuality
                    'set image attributes
                    g.DrawImage(OriginalImage, 0, 0, rect, GraphicsUnit.Pixel)
                    d_foto.Image = _img
                End If
            End If
        Catch ex As Exception
        End Try
    End Sub

    Private Sub recientesfoto_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        MsgBox("hola")
    End Sub

    Private Sub Button14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        For value As Integer = 0 To 8000
            SaveRecord()
            Console.WriteLine(value)
        Next
    End Sub

    Function GridAExcel(ByVal ElGrid As DataGridView) As Boolean

        'Creamos las variables
        Dim exApp As New Microsoft.Office.Interop.Excel.Application
        Dim exLibro As Microsoft.Office.Interop.Excel.Workbook
        Dim exHoja As Microsoft.Office.Interop.Excel.Worksheet
        Try

            'Añadimos el Libro al programa, y la hoja al libro
            exLibro = exApp.Workbooks.Add
            exHoja = exLibro.Worksheets.Add()
            ' ¿Cuantas columnas y cuantas filas?
            Dim NCol As Integer = ElGrid.ColumnCount
            Dim NRow As Integer = ElGrid.RowCount
            pb.Value = 0
            pb.Maximum = NRow
            pb.Step = 1
            'Aqui recorremos todas las filas, y por cada fila todas las columnas
            'y vamos escribiendo.
            For i As Integer = 1 To NCol
                exHoja.Cells.Item(1, i) = ElGrid.Columns(i - 1).Name.ToString
            Next
            For Fila As Integer = 0 To NRow - 1
                For Col As Integer = 0 To NCol - 1
                    
                    exHoja.Cells.Item(Fila + 2, Col + 1) = _
                     ElGrid.Rows(Fila).Cells(Col).Value()
                Next
                pb.PerformStep()
                Application.DoEvents()
                pb.Refresh()
            Next
            'Titulo en negrita, Alineado al centro y que el tamaño de la columna
            'se ajuste al texto
            exHoja.Rows.Item(1).Font.Bold = 1
            exHoja.Rows.Item(1).HorizontalAlignment = 3
            exHoja.Columns.AutoFit()
            'Aplicación visible
            exApp.Application.Visible = True
            exHoja = Nothing
            exLibro = Nothing
            exApp = Nothing
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "Error al exportar a Excel")
            Return False
        End Try
        Return True
    End Function



    Private Sub Button16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button16.Click
        GridAExcel(DataGridView1)
    End Sub

    Private Sub Button14_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button14.Click
        Derecha.Visible = True
        SplitContainer1.Panel1Collapsed = True
        sync()
        Me.FlowLayoutPanel3.Left = Panel4.Width - FlowLayoutPanel3.Width
    End Sub



    Private Sub btn_5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_5.Click
        Application.Exit()
    End Sub

    Private Sub personal_id_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles personal_id.KeyDown
        If e.KeyCode = Keys.Enter Then
            btn_1_Click(sender, New System.EventArgs())
            personal_id.Text = ""
        End If
    End Sub

   
    Private Sub Button19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button19.Click
        idsys()
        abrir("Datos")
    End Sub

    Private Sub personal_id_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles personal_id.TextChanged

    End Sub

    Private Sub Button17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        idsys()
        abrir("Datos")
    End Sub

    Private Sub Button20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button20.Click
        idsys()
        abrir("Datos")
    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick

    End Sub

    Private Sub FlowLayoutPanel1_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles FlowLayoutPanel1.Paint

    End Sub

    Private Sub Button13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button13.Click
        sysRecientes()
    End Sub

    Private Sub Label16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label16.Click

    End Sub

    Private Sub LADOA_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LADOA.Click

    End Sub

    Private Sub LADOB_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LADOB.Click

    End Sub
End Class
