VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "ScenarioApproach"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

'this class contains liabilites, assets and the base + 8 BMA scenarios collections and brings them together
'could be extended to many liabilities since they can be stored in a collection
'container & counter
Private maxTimeProjection As Integer
Private thisScenario As scenario
Private value As Double
Private counter As Integer
Private time As Integer
Private temp As Double
Private count As Integer
'&&&&&&&&&&&&&&&&&&&
Public CumulativeUnmatchedPVScenario0 As Double
Public CumulativeUnmatchedPVScenario1 As Double
Public CumulativeUnmatchedPVScenario2 As Double
Public CumulativeUnmatchedPVScenario3 As Double
Public CumulativeUnmatchedPVScenario4 As Double
Public CumulativeUnmatchedPVScenario5 As Double
Public CumulativeUnmatchedPVScenario6 As Double
Public CumulativeUnmatchedPVScenario7 As Double
Public CumulativeUnmatchedPVScenario8 As Double
'class objects
Public liab As Liability
Public bmaRates As BMAInterestrates
Public Assets As Asset
Public bmascenario As scenario
'Collections and arrays
Private pAssetMinusLiabilities(100)
Public scenarioCollection As Collection
Public pLiabilityCollection As Collection
Public assetcollection As Collection
Public netCFCollection As Collection
Sub AddLiabilityToCollection(value As Liability)

pLiabilityCollection.Add Item:=value, key:=value.ID

End Sub
Property Get LiabilityCollection(key As String) As Liability
LiabilityCollection = pLiabilityCollection(key)
End Property
Public Property Let assetMinusLiabilites(index As Integer, value As Double)
pAssetMinusLiabilities(index) = value
End Property


Public Property Get assetMinusLiabilites(index As Integer) As Double
assetMinusLiabilites = pAssetMinusLiabilities(index)

End Property


Public Sub init()

    Set bmaRates = New BMAInterestrates
    Set liab = New Liability
    Set Assets = New Asset
    'create the required collections to hold objects
    Set scenarioCollection = New Collection
    Set pLiabilityCollection = New Collection
    Set assetcollection = New Collection
 'could be added to collections above later (asset & liabilities will need some ID to be tagged)
    'initialize the standard rates from the BMA curve
     Call Me.bmaRates.init(range("Interest_rates1"), 0)
     'spread above
    ' Me.bmaRates.spreadOverBMARates = 0.0153
     
 'for now A&L are initiated inside the constructor but later they will be added in the collection in other ways
 
     'add the spread above the BMA curve we are entilted to
     Me.bmaRates.addSpreadToBMACurves
    'initializing the liability cf
    Me.liab.init
    Me.pLiabilityCollection.Add Item:=liab, key:=liab.ID
    'initalizing the asset cf
    Me.Assets.init
End Sub
'this works for 1 only but could pass a collection instead (to be revisited)
Sub calculateAssetMinusLiabilites()
Dim i As Integer

    For i = 1 To 100
       pAssetMinusLiabilities(i) = Me.Assets.AssetCF(i) - Me.liab.liabilityCF(i)
    Next i
    
End Sub
Public Function calculateUnmatchedCFPresentValue(ByRef thisScenario As scenario)

    temp = 0
    For count = 1 To UBound(pAssetMinusLiabilities)
        temp = temp + pAssetMinusLiabilities(count) * thisScenario.discountVector(count)
    Next count
    calculateUnmatchedCFPresentValue = temp
End Function

Sub calculateCumulativeUnmatchedPVByScenario()
Me.CumulativeUnmatchedPVScenario0 = Me.CumulativeUnmatchedPVScenario0 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("0"))
Me.CumulativeUnmatchedPVScenario1 = Me.CumulativeUnmatchedPVScenario1 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("1"))
Me.CumulativeUnmatchedPVScenario2 = Me.CumulativeUnmatchedPVScenario2 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("2"))
Me.CumulativeUnmatchedPVScenario3 = Me.CumulativeUnmatchedPVScenario3 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("3"))
Me.CumulativeUnmatchedPVScenario4 = Me.CumulativeUnmatchedPVScenario4 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("4"))
Me.CumulativeUnmatchedPVScenario5 = Me.CumulativeUnmatchedPVScenario5 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("5"))
Me.CumulativeUnmatchedPVScenario6 = Me.CumulativeUnmatchedPVScenario6 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("6"))
Me.CumulativeUnmatchedPVScenario7 = Me.CumulativeUnmatchedPVScenario7 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("7"))
Me.CumulativeUnmatchedPVScenario8 = Me.CumulativeUnmatchedPVScenario8 + Me.calculateUnmatchedCFPresentValue(Me.scenarioCollection("8"))
End Sub


Sub createRequiredScenarios()
 '|||||creating scenario0|||||||
    maxTimeProjection = 100
    'step0: create a scenario object
    For counter = 0 To 8
        Set thisScenario = New scenario
        'step1:create interest vector from the rules of scenario "counter"
        Call Me.bmaRates.generateScenario(counter)
        
            For time = 1 To maxTimeProjection
                'get the value from the scenario object
                value = Me.bmaRates.GetBMACurrentScenarioRate(time)
                'assign the value obtain from the bmaRates object to the scenario object
                thisScenario.BMAScenarioRates(time) = value
            Next time
        'step3: create discount vector
        Call thisScenario.createDiscountVector
        'Step4:assign an integer ID to the scenario
        thisScenario.ID = CStr(counter)
        'adding scenario to the collection
        Me.scenarioCollection.Add Item:=thisScenario, key:=CStr(counter)
    Next counter
End Sub
Sub assignReservesValue()

Me.liab.reserve0 = Me.liab.calculatePresentValue(Me.scenarioCollection("0"))
Me.liab.reserve1 = Me.liab.calculatePresentValue(Me.scenarioCollection("1"))
Me.liab.reserve2 = Me.liab.calculatePresentValue(Me.scenarioCollection("2"))
Me.liab.reserve3 = Me.liab.calculatePresentValue(Me.scenarioCollection("3"))
Me.liab.reserve4 = Me.liab.calculatePresentValue(Me.scenarioCollection("4"))
Me.liab.reserve5 = Me.liab.calculatePresentValue(Me.scenarioCollection("5"))
Me.liab.reserve6 = Me.liab.calculatePresentValue(Me.scenarioCollection("6"))
Me.liab.reserve7 = Me.liab.calculatePresentValue(Me.scenarioCollection("7"))
Me.liab.reserve8 = Me.liab.calculatePresentValue(Me.scenarioCollection("8"))

End Sub
Sub assignReservesValuebyKey(key As String)

Me.pLiabilityCollection.Item(key).reserve0 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("0"))
Me.pLiabilityCollection.Item(key).reserve1 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("1"))
Me.pLiabilityCollection.Item(key).reserve2 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("2"))
Me.pLiabilityCollection.Item(key).reserve3 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("3"))
Me.pLiabilityCollection.Item(key).reserve4 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("4"))
Me.pLiabilityCollection.Item(key).reserve5 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("5"))
Me.pLiabilityCollection.Item(key).reserve6 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("6"))
Me.pLiabilityCollection.Item(key).reserve7 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("7"))
Me.pLiabilityCollection.Item(key).reserve8 = Me.pLiabilityCollection.Item(key).calculatePresentValue(Me.scenarioCollection("8"))


End Sub


Sub outputInterestScenarios()

Dim i As Integer
Dim row As range
Dim count As Integer
Dim column As Integer
Dim bmascenario As scenario
Sheet7.range("A1:Z200").Clear
count = 1
    For Each bmascenario In scenarioCollection
    'since scenario starts at 0
        column = bmascenario.ID + 1

        Sheet7.Cells(1, column) = "Scenario " & bmascenario.ID
        'py
        Sheet7.Cells(1, column + 12) = "Scenario " & bmascenario.ID
        
        For i = 1 To 99
            Sheet7.Cells(i + 1, column) = bmascenario.BMAScenarioRates(i)
            Sheet7.Cells(i + 1, column + 12) = bmascenario.discountVector(i)
        Next i
        
    Next bmascenario
    
    


End Sub



