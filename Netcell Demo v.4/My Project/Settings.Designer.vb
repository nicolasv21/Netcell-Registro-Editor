﻿'------------------------------------------------------------------------------
' <auto-generated>
'     This code was generated by a tool.
'     Runtime Version:4.0.30319.269
'
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>
'------------------------------------------------------------------------------

Option Strict On
Option Explicit On


Namespace My
    
    <Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute(),  _
     Global.System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "10.0.0.0"),  _
     Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)>  _
    Partial Friend NotInheritable Class MySettings
        Inherits Global.System.Configuration.ApplicationSettingsBase
        
        Private Shared defaultInstance As MySettings = CType(Global.System.Configuration.ApplicationSettingsBase.Synchronized(New MySettings()),MySettings)
        
#Region "My.Settings Auto-Save Functionality"
#If _MyType = "WindowsForms" Then
    Private Shared addedHandler As Boolean

    Private Shared addedHandlerLockObject As New Object

    <Global.System.Diagnostics.DebuggerNonUserCodeAttribute(), Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)> _
    Private Shared Sub AutoSaveSettings(ByVal sender As Global.System.Object, ByVal e As Global.System.EventArgs)
        If My.Application.SaveMySettingsOnExit Then
            My.Settings.Save()
        End If
    End Sub
#End If
#End Region
        
        Public Shared ReadOnly Property [Default]() As MySettings
            Get
                
#If _MyType = "WindowsForms" Then
               If Not addedHandler Then
                    SyncLock addedHandlerLockObject
                        If Not addedHandler Then
                            AddHandler My.Application.Shutdown, AddressOf AutoSaveSettings
                            addedHandler = True
                        End If
                    End SyncLock
                End If
#End If
                Return defaultInstance
            End Get
        End Property
        
        <Global.System.Configuration.ApplicationScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.SpecialSettingAttribute(Global.System.Configuration.SpecialSetting.ConnectionString),  _
         Global.System.Configuration.DefaultSettingValueAttribute("data source=c:\Netcell\DB\CedulasDB.db")>  _
        Public ReadOnly Property CedulasDBConnectionString1() As String
            Get
                Return CType(Me("CedulasDBConnectionString1"),String)
            End Get
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_id() As Integer
            Get
                Return CType(Me("Tabla_id"),Integer)
            End Get
            Set
                Me("Tabla_id") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_cedula() As Integer
            Get
                Return CType(Me("Tabla_cedula"),Integer)
            End Get
            Set
                Me("Tabla_cedula") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_nombres() As Integer
            Get
                Return CType(Me("Tabla_nombres"),Integer)
            End Get
            Set
                Me("Tabla_nombres") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_apellidos() As Integer
            Get
                Return CType(Me("Tabla_apellidos"),Integer)
            End Get
            Set
                Me("Tabla_apellidos") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_nacionalidad() As Integer
            Get
                Return CType(Me("Tabla_nacionalidad"),Integer)
            End Get
            Set
                Me("Tabla_nacionalidad") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_sexo() As Integer
            Get
                Return CType(Me("Tabla_sexo"),Integer)
            End Get
            Set
                Me("Tabla_sexo") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_lugar_nacimiento() As Integer
            Get
                Return CType(Me("Tabla_lugar_nacimiento"),Integer)
            End Get
            Set
                Me("Tabla_lugar_nacimiento") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_fecha_nacimiento() As Integer
            Get
                Return CType(Me("Tabla_fecha_nacimiento"),Integer)
            End Get
            Set
                Me("Tabla_fecha_nacimiento") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_profesion() As Integer
            Get
                Return CType(Me("Tabla_profesion"),Integer)
            End Get
            Set
                Me("Tabla_profesion") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_telefono() As Integer
            Get
                Return CType(Me("Tabla_telefono"),Integer)
            End Get
            Set
                Me("Tabla_telefono") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_direccion() As Integer
            Get
                Return CType(Me("Tabla_direccion"),Integer)
            End Get
            Set
                Me("Tabla_direccion") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_fecha_expiracion() As Integer
            Get
                Return CType(Me("Tabla_fecha_expiracion"),Integer)
            End Get
            Set
                Me("Tabla_fecha_expiracion") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_numero_plastico() As Integer
            Get
                Return CType(Me("Tabla_numero_plastico"),Integer)
            End Get
            Set
                Me("Tabla_numero_plastico") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_numero_chip() As Integer
            Get
                Return CType(Me("Tabla_numero_chip"),Integer)
            End Get
            Set
                Me("Tabla_numero_chip") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_cedula_ancho() As Integer
            Get
                Return CType(Me("Tabla_cedula_ancho"),Integer)
            End Get
            Set
                Me("Tabla_cedula_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_nombres_ancho() As Integer
            Get
                Return CType(Me("Tabla_nombres_ancho"),Integer)
            End Get
            Set
                Me("Tabla_nombres_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_apellidos_ancho() As Integer
            Get
                Return CType(Me("Tabla_apellidos_ancho"),Integer)
            End Get
            Set
                Me("Tabla_apellidos_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_nacionalidad_ancho() As Integer
            Get
                Return CType(Me("Tabla_nacionalidad_ancho"),Integer)
            End Get
            Set
                Me("Tabla_nacionalidad_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_sexo_ancho() As Integer
            Get
                Return CType(Me("Tabla_sexo_ancho"),Integer)
            End Get
            Set
                Me("Tabla_sexo_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_lugar_nacimiento_ancho() As Integer
            Get
                Return CType(Me("Tabla_lugar_nacimiento_ancho"),Integer)
            End Get
            Set
                Me("Tabla_lugar_nacimiento_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_fecha_nacimiento_ancho() As Integer
            Get
                Return CType(Me("Tabla_fecha_nacimiento_ancho"),Integer)
            End Get
            Set
                Me("Tabla_fecha_nacimiento_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_profesion_ancho() As Integer
            Get
                Return CType(Me("Tabla_profesion_ancho"),Integer)
            End Get
            Set
                Me("Tabla_profesion_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_telefono_ancho() As Integer
            Get
                Return CType(Me("Tabla_telefono_ancho"),Integer)
            End Get
            Set
                Me("Tabla_telefono_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_direccion_ancho() As Integer
            Get
                Return CType(Me("Tabla_direccion_ancho"),Integer)
            End Get
            Set
                Me("Tabla_direccion_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_fecha_expiracion_ancho() As Integer
            Get
                Return CType(Me("Tabla_fecha_expiracion_ancho"),Integer)
            End Get
            Set
                Me("Tabla_fecha_expiracion_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_numero_plastico_ancho() As Integer
            Get
                Return CType(Me("Tabla_numero_plastico_ancho"),Integer)
            End Get
            Set
                Me("Tabla_numero_plastico_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_numero_chip_ancho() As Integer
            Get
                Return CType(Me("Tabla_numero_chip_ancho"),Integer)
            End Get
            Set
                Me("Tabla_numero_chip_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_email() As Integer
            Get
                Return CType(Me("Tabla_email"),Integer)
            End Get
            Set
                Me("Tabla_email") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_observaciones() As Integer
            Get
                Return CType(Me("Tabla_observaciones"),Integer)
            End Get
            Set
                Me("Tabla_observaciones") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_direccion2() As Integer
            Get
                Return CType(Me("Tabla_direccion2"),Integer)
            End Get
            Set
                Me("Tabla_direccion2") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_telefono2() As Integer
            Get
                Return CType(Me("Tabla_telefono2"),Integer)
            End Get
            Set
                Me("Tabla_telefono2") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("1")>  _
        Public Property Tabla_celular2() As Integer
            Get
                Return CType(Me("Tabla_celular2"),Integer)
            End Get
            Set
                Me("Tabla_celular2") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_email_ancho() As String
            Get
                Return CType(Me("Tabla_email_ancho"),String)
            End Get
            Set
                Me("Tabla_email_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_observaciones_ancho() As String
            Get
                Return CType(Me("Tabla_observaciones_ancho"),String)
            End Get
            Set
                Me("Tabla_observaciones_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_direccion2_ancho() As String
            Get
                Return CType(Me("Tabla_direccion2_ancho"),String)
            End Get
            Set
                Me("Tabla_direccion2_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_telefono2_ancho() As String
            Get
                Return CType(Me("Tabla_telefono2_ancho"),String)
            End Get
            Set
                Me("Tabla_telefono2_ancho") = value
            End Set
        End Property
        
        <Global.System.Configuration.UserScopedSettingAttribute(),  _
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
         Global.System.Configuration.DefaultSettingValueAttribute("50")>  _
        Public Property Tabla_celular2_ancho() As String
            Get
                Return CType(Me("Tabla_celular2_ancho"),String)
            End Get
            Set
                Me("Tabla_celular2_ancho") = value
            End Set
        End Property
    End Class
End Namespace

Namespace My
    
    <Global.Microsoft.VisualBasic.HideModuleNameAttribute(),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute()>  _
    Friend Module MySettingsProperty
        
        <Global.System.ComponentModel.Design.HelpKeywordAttribute("My.Settings")>  _
        Friend ReadOnly Property Settings() As Global.Netcell_Demo_v._4.My.MySettings
            Get
                Return Global.Netcell_Demo_v._4.My.MySettings.Default
            End Get
        End Property
    End Module
End Namespace