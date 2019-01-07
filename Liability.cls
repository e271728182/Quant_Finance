VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Liability"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
'container & counter
Private i As Integer
Private temp As Double
Private count As Integer
Private row As range
''''''''''''''''''''''''
Private pLiabilityVector(100) As Double
'to create later an asset list tied to a given liability if necessary
Private pAssetListID() As Collection
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Public isInterestRateDependant As Boolean
Public reserve_risk As Double
Public longevity_risk As Double
Public naar As Double

Public frequency As Integer
Public ID As String
Public reserve0 As Double
Public reserve1 As Double
Public reserve2 As Double
Public reserve3 As Double
Public reserve4 As Double
Public reserve5 As Double
Public reserve6 As Double
Public reserve7 As Double
Public reserve8 As Double




Public Property Get liabilityCF(index As Integer) As Double
liabilityCF = pLiabilityVector(index)

End Property

Public Property Let liabilityCF(index As Integer, value As Double)
pLiabilityVector(index) = value
End Property


Sub init()
Dim row As range
Dim count As Integer
count = 1
Me.frequency = 1
'assigning Liability CF values to the vector
Me.loadCF

'assigning a key for later ID
Me.ID = "ABCDE"
Me.isInterestRateDependant = False
End Sub

Sub loadCF()

'load liability CF in a vector for later computations
count = 1
    For Each row In range("liability_input_range").Rows
        If count > 100 Then Exit For
       'counter to track the number of rows
       temp = temp + 1
       'adding cf to vector of a given year
       pLiabilityVector(count) = pLiabilityVector(count) + row.Cells(1)
    
        'increasing the counter only by steps of frequency of CF (monthly=12, annual=1,biannual=2 etc)
        If temp Mod Me.frequency = 0 Then
        count = Application.WorksheetFunction.Min(count + 1, 99)
        Else
        count = count
        End If
    Next row
    
End Sub
Public Function calculatePresentValue(ByRef thisScenario As scenario)

    temp = 0
    For count = 1 To UBound(pLiabilityVector)
        temp = temp + pLiabilityVector(count) * thisScenario.discountVector(count)
    Next count
    calculatePresentValue = temp
End Function


Sub dumpCF()
For i = 1 To UBound(pLiabilityVector)
range("dump").Cells(i) = Me.liabilityCF(i)
Next i
End Sub
Function dumpAllCF()

dumpAllCF = pLiabilityVector

End Function
