VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "BMAInterestrates"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

'this class contains the standard BMA curve with the mechanics to create the addtional one which are stored into a vector called BMACurrentScenarioRates
Private temp As Double
Private temp2 As Double
Private temp3 As Double
Private temp4 As Double
Private count As Integer
Private i As Integer
Private futureT As Integer
Private pBMAStandardRates(100) As Double
Private pBMAStandardRates10(90) As Double
Private pDiscountVector(100) As Double
Private pBMACurrentScenarioRates(100) As Double
Private pBMACurrentScenario10(90) As Double
Private pSpread(100) As Double
Private maxtime As Integer
Public key As String

Public Property Let spreadOverBMARates(value As Double)
pSpread = value
End Property


Public Property Get spreadOverBMARates() As Double
For Each row In range("Spread_Range").Rows

'counter to track the number of rowstemp = temp + 1
       'adding cf to vector of a given year
pBDASpread(count) = row.Cells(1)

Next row
End Property

Public Property Let LetBMAStandardRates(index As Integer, value As Double)
DiscountVectors(index) = value
End Property

'property to access the values in the vector
Public Property Get GetBMAStandardRates(value As Integer) As Double
GetBMAStandardRates = pGetBMAStandardRates(value)

End Property
'property to access the values in the vector
Public Property Get GetBMACurrentScenarioRate(value As Integer) As Double
GetBMACurrentScenarioRate = pBMACurrentScenarioRates(value)

End Property

Public Sub init(interestRateRange As range, spread As Double)
    Dim row As range

    count = 1
    'assigning interest rates values to the vector
    For Each row In interestRateRange.Rows
        'for the original standard rates
        pBMAStandardRates(count) = row.Cells(1)
        
        'the current scenario at inception is by default the standard rates
        pBMACurrentScenarioRates(count) = row.Cells(1)
        'increasing the counter
        count = count + 1
    Next row
    count = 1
    For Each row In range("Spread_Range").Rows

    'counter to track the number of rowstemp = temp + 1
       'adding cf to vector of a given year
    pSpread(count) = row.Cells(1)
    count = count + 1
    Next row
    
    'assigning a key for later ID
    Me.key = "ABCDE"
    
     'Me.createFutureSpot

End Sub
Sub dumpCF(ByRef vector, range)
For i = 1 To UBound(pLiabilityVector)
range(range).Cells(i) = vector(i)
Next i
End Sub
Public Sub createFutureSpot()
    futureT = 10
    For i = 1 To UBound(pBMAStandardRates10)

        temp = (1 + pBMAStandardRates(futureT)) ^ futureT
        'find forward 1 year rate from original spot curve
        temp2 = (1 + pBMAStandardRates(futureT + i)) ^ (futureT + i)

        temp4 = (temp2 / temp) ^ (1 / i) - 1
        pBMAStandardRates10(i) = temp4
    
    Next i
    
End Sub

Public Sub addSpreadToBMACurves()

    For i = 1 To UBound(pBMAStandardRates)
               'Simply the BMA standard Rates
        pBMACurrentScenarioRates(i) = pBMACurrentScenarioRates(i) + pSpread(i)
        pBMAStandardRates(i) = pBMAStandardRates(i) + pSpread(i)
    Next i

End Sub

Sub createDiscountVector()
'Dim count As Integer
discountVector(0) = 1

'creating a discount curve
For count = 1 To UBound(BMAStandardRates)
pDiscountVector(count) = pDiscountVector(count - 1) / (1 + pBMACurrentScenarioRates(count))
Next count
End Sub
Public Sub generateScenario(value As Integer)
'get an integer as in input that tells you which scenario between 0-8 to compute
Select Case value

Case 0
    For i = 1 To UBound(pBMAStandardRates)

        'Simply the BMA standard Rates
        pBMACurrentScenarioRates(i) = pBMAStandardRates(i)
    Next i
Case 1
    For i = 1 To UBound(pBMAStandardRates)
        'take the minimum between 1.5% and the linear value of 1.5% that decreases over 10 years
        temp = WorksheetFunction.Min(0.015 * i / 10, 0.015)
        'substract the temp value to the bermuda rates
        pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) - temp)
    Next i
    
Case 2
    For i = 1 To UBound(pBMAStandardRates)
        'take the minimum between 1.5% and the linear value of 1.5% that decreases over 10 years
        temp = WorksheetFunction.Min(0.015 * i / 10, 0.015)
        'add the temp value to the bermuda rates
        pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
    Next i
    
Case 3
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 5
            'take the minimum between 1.5% and the linear value of 1.5% that decreases over 5 years
            temp = WorksheetFunction.Min(0.015 * i / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) - temp)
        
        Case Else
            'Start at -1.5% and and increase linearly over 5 yeats
            temp = -0.015 + WorksheetFunction.Min(0.015 * (i - 5) / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i
    

Case 4
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 5
            'take the minimum between 1.5% and the linear value of 1.5% that decreases over 5 years
            temp = WorksheetFunction.Min(0.015 * i / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        
        Case Else
            'Start at 1.5% and and decrease linearly over 5 yeats
            temp = 0.015 - WorksheetFunction.Min(0.015 * (i - 5) / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i


Case 5

    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 10
            ''i. Year 1 spot rate -1.5% that goes to -1% after 10 years
            temp = -0.015 + 0.005 * i / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        
        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -0.5%
            temp = -0.01 + 0.005 * (i - 10) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        Case Else
            temp = -0.005
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i

Case 6
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 10
            ''i. Year 1 spot rate -0.5% that goes to -1% after 10 years
            temp = -0.005 - 0.005 * (i - 1) / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        
        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -1.5%
            temp = -0.01 + -0.005 * (i - 11) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        Case Else
            'iii. Year 30 spot rate -1.5%
            temp = -0.015
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i

'Case 7
'    For i = 1 To UBound(pBMAStandardRates)
'        Select Case i
'
'        Case Is <= 10
'            ''i. Year 1 spot rate 0.5% that goes to 1% after 10 years
'            temp = 0.005 + 0.005 * (i - 1) / 10
'            'add the temp value to the bermuda rates
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'
'        Case Is <= 30
'            'Start at 1% and and increase linearly over 20 years to +1.5%
'            temp = 0.01 + 0.005 * (i - 11) / 20
'            'add the temp value to the bermuda rates
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'        Case Else
'            'iii. Year 30 spot rate +1.5%
'            temp = 0.015
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'        End Select
'    Next i
'Case 8
'
'    For i = 1 To UBound(pBMAStandardRates)
'        Select Case i
'
'        Case Is <= 10
'        temp = 0
'        pBMACurrentScenarioRates(i) = pBMAStandardRates(i)
'        Case Is <= 20
'            ''i. Year 1 spot rate -1.5% that goes to -1% after 10 years
'            temp = 0.015 - 0.005 * (i - 11) / 10
'            'add the temp value to the bermuda rates
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'
'        Case Is <= 31
'            'Start at -1% and and decrease linearly over 20 years to -0.5%
'            temp = 0.01 - 0.005 * (i - 21) / 20
'            'add the temp value to the bermuda rates
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'        Case Else
'            temp = 0.005
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'        End Select
'    Next i
Case 7

    For i = 1 To UBound(pBMAStandardRates10)
        Select Case i

        Case Is <= 10
            ''i. Year 1 spot rate -1.5% that goes to -1% after 10 years
            temp = -0.0155 + 0.005 * (i) / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)

        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -0.5%
            temp = -0.01025 + 0.005 * (i - 10) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)
        Case Else
            temp = -0.005
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)
        End Select
    Next i

    ' recreate the time 0 structure
    For i = 1 To UBound(pBMAStandardRates)
        
        Select Case i
        Case Is <= 10
        pBMACurrentScenarioRates(i) = pBMAStandardRates(i)

        Case Else
        'forward rate between 0-10 is spot compounded
        temp = (1 + pBMACurrentScenarioRates(10)) ^ 10
        maxtime = Application.WorksheetFunction.Min(90, i - futureT)
        temp2 = (1 + pBMACurrentScenario10(maxtime)) ^ (maxtime)
        temp3 = (temp * temp2) ^ (1 / i)
        pBMACurrentScenarioRates(i) = temp3 - 1

        End Select
        
        Next i



Case 8
    For i = 1 To UBound(pBMAStandardRates10)
        Select Case i

        Case Is <= 10
            ''i. Year 1 spot rate -1.5% that goes to -1% after 10 years
            temp = 0.0155 - 0.005 * (i) / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)

        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -0.5%
            temp = 0.01025 - 0.005 * (i - 10) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)
        Case Else
            temp = 0.005
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)
        End Select
    Next i

    ' recreate the time 0 structure
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        Case Is <= 10
        pBMACurrentScenarioRates(i) = pBMAStandardRates(i)

        Case Else
        'forward rate between 0-10 is spot compounded
        temp = (1 + pBMACurrentScenarioRates(10)) ^ 10
        maxtime = Application.WorksheetFunction.Min(90, i - futureT)
        temp2 = (1 + pBMACurrentScenario10(maxtime)) ^ (maxtime)
        temp3 = (temp * temp2) ^ (1 / i)
        pBMACurrentScenarioRates(i) = temp3 - 1

        End Select
        Next i
    For i = 1 To UBound(pBMAStandardRates10)
    
        range("ForwardTerm").Cells(i) = pBMAStandardRates10(i)
        range("ForwardTermShock").Cells(i) = pBMACurrentScenario10(i)
       Next i
       
End Select

End Sub



Sub Scenario0()
'This is the base case
   ' Dim i As Integer
    Dim temp As Double
    
    For i = 1 To UBound(pBMAStandardRates)

        'Simply the BMA standard Rates
        pBMACurrentScenarioRates(i) = pBMAStandardRates(i)
    Next i
End Sub
Sub Scenario1()
'All rates decrease annually to total decrease of 1.5% in tenth year; unchanged
'thereafter.
  '  Dim i As Integer
    Dim temp As Double
    
    For i = 1 To UBound(pBMAStandardRates)
        'take the minimum between 1.5% and the linear value of 1.5% that decreases over 10 years
        temp = WorksheetFunction.Min(0.015 * i / 10, 0.015)
        'substract the temp value to the bermuda rates
        pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) - temp)
    Next i
    
    
        
End Sub
Sub Scenario2()
'All rates increase annually to total increase of 1.5% in tenth year; unchanged
'thereafter
   ' Dim i As Integer
    Dim temp As Double
    
    For i = 1 To UBound(pBMAStandardRates)
        'take the minimum between 1.5% and the linear value of 1.5% that decreases over 10 years
        temp = WorksheetFunction.Min(0.015 * i / 10, 0.015)
        'add the temp value to the bermuda rates
        pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
    Next i
    


End Sub

Sub Scenario3()
'All rates decrease annually to total decrease of 1.5% in fifth year then back up
'again by tenth year

   ' Dim i As Integer
    Dim temp As Double
    
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 5
            'take the minimum between 1.5% and the linear value of 1.5% that decreases over 5 years
            temp = WorksheetFunction.Min(0.015 * i / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) - temp)
        
        Case Else
            'Start at -1.5% and and increase linearly over 5 yeats
            temp = -0.015 + WorksheetFunction.Min(0.015 * (i - 5) / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i
    


End Sub


Sub Scenario4()
'All rates increase annually to total increase of 1.5% in fifth year then back down
'again by tenth year
'Dim i As Integer
Dim temp As Double
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 5
            'take the minimum between 1.5% and the linear value of 1.5% that decreases over 5 years
            temp = WorksheetFunction.Min(0.015 * i / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        
        Case Else
            'Start at 1.5% and and decrease linearly over 5 yeats
            temp = 0.015 - WorksheetFunction.Min(0.015 * (i - 5) / 5, 0.015)
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i


End Sub
Sub Scenario5()

'Decrease with positive twist to the following net change after ten years (interpolate
'for other durations):
'i. Year 1 spot rate -1.5%
'ii. Year 10 spot rate -1.0%
'iii. Year 30 spot rate -0.5% r
Dim temp As Double
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 10
            ''i. Year 1 spot rate -1.5% that goes to -1% after 10 years
            temp = -0.015 + 0.005 * i / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        
        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -0.5%
            temp = -0.01 + 0.005 * (i - 10) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        Case Else
            temp = -0.005
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i

End Sub

Sub Scenario6()
'Decrease with negative twist to the following net change after for ten years
'interpolate for other durations):
'i. Year 1 spot rate -0.5%
'ii. Year 10 spot rate -1.0%
'iii. Year 30 spot rate -1.5%
Dim temp As Double
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 10
            ''i. Year 1 spot rate -0.5% that goes to -1% after 10 years
            temp = -0.005 - 0.005 * i / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        
        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -1.5%
            temp = -0.01 + -0.005 * (i - 10) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        Case Else
            'iii. Year 30 spot rate -1.5%
            temp = -0.015
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i


End Sub

Sub Scenario7()

'Increase with positive twist to the following net change after ten years (interpolate
'for other durations):
'i. Year 1 spot rate +0.5%
'ii. Year 10 spot rate +1.0%
'iii. Year 30 spot rate +1.5%
Dim temp As Double
    For i = 1 To UBound(pBMAStandardRates)
        Select Case i
        
        Case Is <= 10
            ''i. Year 1 spot rate 0.5% that goes to 1% after 10 years
            temp = 0.005 + 0.005 * i / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        
        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -1.5%
            temp = 0.01 + 0.005 * (i - 10) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        Case Else
            'iii. Year 30 spot rate -1.5%
            temp = 0.015
            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
        End Select
    Next i

End Sub


Sub Scenario8()
'Increase with negative twist to the following net change after for ten years (interpolate
'for other durations):
'i. Year 1 spot rate +1.5%
'ii. Year 10 spot rate +1.0%
'iii. Year 30 spot rate +0.5%
Dim temp As Double
'    For i = 1 To UBound(pBMAStandardRates)
'        Select Case i
'
'        Case Is <= 10
'            ''i. Year 1 spot rate -1.5% that goes to -1% after 10 years
'            temp = 0.015 - 0.005 * i / 10
'            'add the temp value to the bermuda rates
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'
'        Case Is <= 30
'            'Start at -1% and and decrease linearly over 20 years to -0.5%
'            temp = 0.01 + 0.005 * (i - 10) / 20
'            'add the temp value to the bermuda rates
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'        Case Else
'            temp = 0.005
'            pBMACurrentScenarioRates(i) = WorksheetFunction.Max(0, pBMAStandardRates(i) + temp)
'        End Select
'    Next i
    For i = 1 To UBound(pBMAStandardRates10)
        Select Case i

        Case Is <= 10
            ''i. Year 1 spot rate -1.5% that goes to -1% after 10 years
            temp = 0.015 - 0.005 * i / 10
            'add the temp value to the bermuda rates
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)

        Case Is <= 30
            'Start at -1% and and decrease linearly over 20 years to -0.5%
            temp = 0.01 + 0.005 * (i - 10) / 20
            'add the temp value to the bermuda rates
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)
        Case Else
            temp = 0.005
            pBMACurrentScenario10(i) = WorksheetFunction.Max(0, pBMAStandardRates10(i) + temp)
        End Select
    Next i
    
 
End Sub

