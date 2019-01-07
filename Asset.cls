VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Asset"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
'this class contains a vector of cf for a given asset with an ID to track it
'container & counter
Private count As Integer
Private row As range
Private temp As Double
Private i As Integer
''''''''''''''''''''''
Public frequency As Integer
Public marketValue As Double
Private pAssetVector(100) As Double
Public ID As String
Public Property Let AssetCF(index As Integer, value As Double)
pAssetVector(index) = value
End Property

Public Property Get AssetCF(index As Integer) As Double
AssetCF = pAssetVector(index)

End Property

Sub init()
Dim row As range
Dim count As Integer
count = 1
'assigning asset cf to the vector
Me.frequency = 1
Me.loadCF
'Me.marketValue = range("asset_input_range").Cells(2, 1)

'assigning a key for later ID
Me.ID = range("asset_input_range").Cells(1, 1)

End Sub
Sub loadCF()

'load liability CF in a vector for later computations
count = 1
    For Each row In range("asset_input_range").Rows
       'first row is ID and second row is the Marker value
       If count > 100 Then Exit For
       Select Case count
       
       Case Is > 2
       
           'counter to track the number of rows
           temp = temp + 1
           'adding cf to vector of a given year
           pAssetVector(count - 2) = pAssetVector(count - 2) + row.Cells(1)
        
            'increasing the counter only by steps of frequency of CF (monthly=12, annual=1,biannual=2 etc)
            If temp Mod Me.frequency = 0 Then
            count = count + 1
            Else
            count = count
            End If
        Case Else
            count = count + 1
        
        End Select
        
    Next row
    
End Sub
Sub dumpCF()
For i = 1 To UBound(pAssetVector)

range("asset_dump").Cells(i) = Me.AssetCF(i)

Next i


End Sub
