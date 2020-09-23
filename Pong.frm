VERSION 5.00
Object = "{08216199-47EA-11D3-9479-00AA006C473C}#2.1#0"; "RMCONTROL.OCX"
Begin VB.Form Form1 
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Pong3D - Steve Ciokan - <<SPACE>> Changes Views"
   ClientHeight    =   7170
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   9660
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7170
   ScaleWidth      =   9660
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer PongTime 
      Interval        =   10
      Left            =   120
      Top             =   120
   End
   Begin RMControl7.RMCanvas RMCanvas1 
      Height          =   5175
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   9015
      _ExtentX        =   15901
      _ExtentY        =   9128
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim PongFrame As Direct3DRMFrame3
Dim Pad1 As Direct3DRMMeshBuilder3
Dim Pad2 As Direct3DRMMeshBuilder3
Dim wall1 As Direct3DRMMeshBuilder3
Dim wall2 As Direct3DRMMeshBuilder3
Dim Ball As Direct3DRMMeshBuilder3
Dim BallX, BallY, BallSpeed As Single
Dim Pad1X, Pad1Y, Pad1Speed As Single
Dim Pad2X, Pad2Y, Pad2Speed As Single
Dim i, view As Integer
Dim ballup, ballleft As Boolean

Private Sub Form_Load()

Me.Height = RMCanvas1.Height + 300
Me.Width = RMCanvas1.Width


RMCanvas1.StartWindowed
RMCanvas1.SceneFrame.SetSceneBackground &HFFFFFF

ballup = True
ballleft = True
BallSpeed = 0.05
Pad1Speed = 0.05
Pad2Speed = 0.05
view = 1
Pong3D_objectCreation

End Sub

Private Sub Pong3D_objectCreation()


Set PongFrame = RMCanvas1.D3DRM.CreateFrame(RMCanvas1.SceneFrame)
Set lights = RMCanvas1.D3DRM.CreateLight(D3DRMLIGHT_DIRECTIONAL, &HFFFFFFFF)

Set Pad1 = RMCanvas1.D3DRM.CreateMeshBuilder
With Pad1
    .LoadFromFile App.Path & "\pongpad.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
    .ScaleMesh 0.01, 0.07, 0.03
    .Translate -3.9, 0, 0
    .SetColorRGB 0, 0, 15
End With


Set Pad2 = RMCanvas1.D3DRM.CreateMeshBuilder
With Pad2
    .LoadFromFile App.Path & "\pongpad.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
    .ScaleMesh 0.01, 0.07, 0.03
    .Translate 3.9, 0, 0
    .SetColorRGB 0, 0, 15
End With

Set wall1 = RMCanvas1.D3DRM.CreateMeshBuilder
With wall1
    .LoadFromFile App.Path & "\pongpad.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
    .ScaleMesh 0.3, 0.01, 0.03
    .Translate 0.15, -2.5, 0
End With

Set wall2 = RMCanvas1.D3DRM.CreateMeshBuilder
With wall2
    .LoadFromFile App.Path & "\pongpad.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
    .ScaleMesh 0.3, 0.01, 0.03
    .Translate 0.15, 2.5, 0
End With

Set Ball = RMCanvas1.D3DRM.CreateMeshBuilder
With Ball
    .LoadFromFile App.Path & "\pongBall.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
    .ScaleMesh 0.015, 0.015, 0.015
    .Translate 0, 0, -0.7
    .SetColorRGB 15, 0, 0
End With

RMCanvas1.AmbientLight.SetColorRGB 0.5, 0.5, 0.5

PongFrame.AddVisual Pad1
PongFrame.AddVisual Pad2
PongFrame.AddVisual wall1
PongFrame.AddVisual wall2
PongFrame.AddVisual Ball


End Sub

Private Sub PongTime_Timer()

i = i + 1
If i > 200 Then
    BallSpeed = BallSpeed + 0.01
    i = 0
End If
ballMove
HitCheck
ComputerMove
If view = 3 Then
     PongFrame.AddRotation D3DRMCOMBINE_REPLACE, 1, 0, 0, 1
ElseIf view = 2 Then
     PongFrame.AddRotation D3DRMCOMBINE_REPLACE, 0, 1, 0, 1
ElseIf view = 1 Then
     PongFrame.AddRotation D3DRMCOMBINE_REPLACE, 0, 0, 0, 0
End If

RMCanvas1.Update


End Sub

Private Sub RMCanvas1_KeyDown(keyCode As Integer, Shift As Integer)
If keyCode = vbKeyDown Then
    Pad1Y = Pad1Y + Pad1Speed
    Pad1.Translate 0, (Pad1Speed * -1), 0
End If
If keyCode = vbKeyUp Then
    Pad1Y = Pad1Y - Pad1Speed
    Pad1.Translate 0, Pad1Speed, 0
End If

If keyCode = vbKeySpace Then
    view = view + 1
    If view = 4 Then view = 1
End If
End Sub

Private Sub ballMove()
If ballup = True Then
    BallY = BallY - BallSpeed
    Ball.Translate 0, BallSpeed, 0
Else
    BallY = BallY + BallSpeed
    Ball.Translate 0, (BallSpeed * -1), 0
End If

If ballleft = True Then
    BallX = BallX - BallSpeed
    Ball.Translate BallSpeed, 0, 0
Else
    BallX = BallX + BallSpeed
    Ball.Translate (BallSpeed * -1), 0, 0
End If
End Sub
Private Sub HitCheck()
If BallY < -2 Then ballup = False
If BallY > 2 Then ballup = True

If BallX > 3.4 Then
    If BallY > Pad1Y - 1 And BallY < Pad1Y + 1 Then
        ballleft = True
    Else
        MsgBox "miss"
        ballleft = True
    End If
End If

If BallX < -3.3 Then
    If BallY > Pad2Y - 1 And BallY < Pad2Y + 1 Then
        ballleft = False
    Else
        MsgBox "miss"
        ballleft = False
    End If
End If
End Sub
Private Sub ComputerMove()

If BallY > Pad2Y Then
    Pad2Y = Pad2Y + Pad2Speed
    Pad2.Translate 0, (Pad2Speed * -1), 0
End If
    
If BallY < Pad2Y Then
    Pad2Y = Pad2Y - Pad2Speed
    Pad2.Translate 0, Pad2Speed, 0
End If

End Sub

