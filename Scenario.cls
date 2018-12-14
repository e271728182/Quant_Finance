VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Scenario"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
'this class create a single scenario which contains a corresponding interest rate vector, a discount vector and
'and an ID for tagging

Private pDiscountVector(100) As Double
Private pBMAScenarioRates(100) As Double
Public ID As String
Public pvLiabilities As Double
'property to change the values of the rates
Public Property Let BMAScenarioRates(index As Integer, value As Double)
pBMAScenarioRates(index) = value
End Property
'property to access the values in the vector
Public Property Get BMAScenarioRates(index As Integer) As Double
BMAScenarioRates = pBMAScenarioRates(index)
End Property
Public Property Let discountVector(index As Integer, value As Double)
pDiscountVector(index) = value
End Property
'property to access the values in the vector
Public Property Get discountVector(index As Integer) As Double
discountVector = pDiscountVector(index)
End Property


Sub createDiscountVector()
'sub to create the discount curve from the rates
Dim count As Integer
discountVector(0) = 1

'creating a discount curve
For count = 1 To UBound(pBMAScenarioRates)
discountVector(count) = (1 + pBMAScenarioRates(count)) ^ -count
Next count
End Sub
