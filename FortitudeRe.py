
from scipy.optimize import newton
import numpy as np
import pandas as pd
import json

def sumNestedDict(dictio,key):
    _total=0
    for value in dictio.values():
        _total=_total+value[key]
    return _total
def obtainFxSpot(dframe,currencyKey,usdKey,lclKey):
    #create a dictionary of spot fx rates from a dataframe of market values
    df1=dframe.groupby(currencyKey).agg({lclKey: "sum",usdKey: "sum"})
    df1['fx']=df1[lclKey]/df1[usdKey]
    df1=df1[['fx']]
    return df1.to_dict()['fx']

def indexBasedDict(dateDictio,dateCf,dateCfKey):
 #given  a key=date value=cf dictionary transform the key to an index based on a
 #dictionary of key=date, value=numerical index
    return {dateDictio[k]:v[dateCfKey] for (k,v) in dateCf.items()}

def createDateDict(dictList):
#create a set of unique Dates to later create a dictionary where date=key integer=value
    uniqueDates=set()
    for item in dictList:

        uniqueDates.update(set(item.keys()))


    uniqueDates=sorted(list(uniqueDates))
    dateDict={}
    count=1
    for value in uniqueDates:
        dateDict[value]=count
        count=count+1
    return dateDict


def convertCFUsdToLcl(exRate,assetCf):
    return {k:v*exRate for (k,v) in assetCf.items()}
class asset:
    def __init__(self):
        self.isin='abc'
        self.mvLclcurrent=0
        self.mvUsdcurrent=0
        self.isGrouped=False
        self.bvLcl=0
        self.mvLcl=0
        self.mvUsd=0
        self.bvUsd=0
        self.valDate='1900-01-01'
        self.cashFlowsLclcurrent={}
        self.cashFlowsUsdCurrent={}
        self.exRateUsdToLcl=1


#Base class for every calculation that is basically a set of asset CF+its key financial info and a liablity

class report:
    def __init__(self):
        self.valuationDate=None
        self.assetList=[]
        #market value asset usd
        self.mvUsd=0
        #market value to use for calculation
        self.mv=0
        #book value to use for calculation
        self.bv=0
        #cash to use for calculation
        self.cash=0
        #frequency of calculation 1=annual 4=quaterly 12=monthly
        self.frequency=12
        self.assetVariableName=('date','assetCf')
        self.liabilityVariableName=('date','liabilityCf')
        #liability cf dictionary used for calculation
        self.liability={k:0.0 for k in range(1,105*self.frequency)}
        #asset cash flow used for calculation
        self.asset={k:0.0 for k in range(1,105*self.frequency)}
        #asset cash flow USD
        self.assetUsd={k:0.0 for k in range(1,105*self.frequency)}

        self.defaultRate=0.00
        self.haircut=1.0
        self.expense=0.000
        #exchnge rate used if necessary to convert USD to other currency
        self.exchangeRateUsdToOther=1

        self.adjustedAsset={}
        self.adjVectorCF={}
        self.alm={}

#accessor funtion to assign the right mv,asset vector and liability for calculations
    def assignMv(self,mvToUse):
        self.mv=mvToUse

    def assignAssets(self,assetToUse):
        self.asset=assetToUse

    def assignLiablity(self,liabilityToUse):
        self.liability=liabilityToUse

#to calculate the IRR of the asset & mv
    def portfolioIrrC(self):
        __values=list(self.asset.values())
        __values.insert(0,-self.mv)
        return np.irr(__values)

#to adjust asset for default and expenses

    def defaultAdjVectorC(self,adjFactor):
        __adjFactor=adjFactor
        __values={}
        for n in range(1,len(self.asset)+1):
            if n==1:
                __values[n]=__adjFactor
            else:
                __values[n]=__values[n-1]*__adjFactor
        return __values


#adjust the asset cash flow by the adjustment vector of default,haircut & expenses
    def adjCfC(self):
        return {k:v*self.adjVectorCF[k] for (k,v) in self.asset.items()}

    def almAdjAssetC(self):
        return {k:self.adjustedAsset[k]-v for (k,v) in self.liability.items()}

class lossRec(report):
    def __init__(self):

        report.__init__(self)
        #create a function that makes them equal lenght before starting to run the program
        self.fixedAssets={k:0.0 for k in range(1,105*self.frequency)}

        self.dynamicAssets={k:0.0 for k in range(1,105*self.frequency)}
        self.bondAmorSchedule={k:0.0 for k in range(1,105*self.frequency)}
        self.percentage=0.5
        self.liabilitySwapNotional={k:0.0 for k in range(1,105*self.frequency)}
        self.liborUk6m={k:0.0 for k in range(1,105*self.frequency)}
        self.bookValuebonds={k:0.0 for k in range(1,105*self.frequency)}
        self.lgTermReinvRates={k:0.0 for k in range(1,105*self.frequency)}
        self.liborSpread=0.0075
        self.maxyear=70

    #gp:maybe replace all these dict with generator functions once the code is debugged
        self.liborReinvAmt=dict()
        self.lgTermReinvAmt=dict()
        self.totalReinvAmt=dict()
        self.availForReinv=dict()
        self.liborEarning=dict()
        self.lgTermEarning=dict()
        self.totalEarning=dict()
        self.endCf=dict()
        self.liborFreqSpreadAdj=dict()
        self.lgTermReinvRatesAdj={k:0.00/12 for k in range(1,105*self.frequency)}
        self.weightVc=dict()
        self.reinvAssets=dict()
        self.shadowLoss=0
    def totalCf(self):
        return {k:v+self.percentage*self.dynamicAssets[k] for (k,v) in self.fixedAssets.items()}

    def liborFreqSpreadAdjC(self):
        return {k:(1+v+self.liborSpread)**(1/self.frequency)-1 for (k,v) in self.liborUk6m.items()}

    def bookValuebondsC(self):
        #create a bond amortization schedule based on the fixed assumptions
        __values={}
        for k,v in self.bondAmorSchedule.items():
            if k==1:
                __values[k]=self.bv*v
            else:
                __values[k]=__values[k-1]*v
        return __values

    def accumulateCf(self):
        self.totalReinvAmt[0]=0
        self.lgTermReinvAmt[0]=0
        self.liborReinvAmt[0]=0
        self.reinvAssets[0]=0
        self.endCf[0]=0
        for k in self.liability.keys():
            #track reinvestable amount


            self.liborEarning[k]=self.liborReinvAmt[k-1]*self.liborFreqSpreadAdj[k]
            self.lgTermEarning[k]=self.lgTermReinvAmt[k-1]*self.lgTermReinvRatesAdj[k]
            #add those amount to total earned
            self.totalEarning[k]=self.liborEarning[k]+self.lgTermEarning[k]
            #at end of period
            self.reinvAssets[k]=self.endCf[k-1]+self.totalEarning[k]
            self.totalReinvAmt[k]=self.reinvAssets[k]+self.alm[k]
            #update the amount availablr for reinvestment
            self.availForReinv[k]=self.reinvAssets[k]+self.alm[k]+min(0,self.bookValuebonds[k]-self.liabilitySwapNotional[k])
            #track the part available for long term reinvestment at 4.75%
            self.lgTermReinvAmt[k]=max(self.availForReinv[k],0)
            #track the part reinvested at libor
            self.liborReinvAmt[k]=self.reinvAssets[k]+self.alm[k]-self.lgTermReinvAmt[k]
            #calculate the interest earned on the libor & 4.75% part
            #end of period CF
            self.endCf[k]=self.endCf[k-1]+self.alm[k]+self.totalEarning[k]

    def weightVcC(self):
        return {k:self.liborReinvAmt[k]/(self.liborReinvAmt[k]+v) for (k,v) in self.liabilitySwapNotional.items()}


    def performLossRec(self,guess):
        self.percentage=guess
        self.asset=self.totalCf()

        self.liborFreqSpreadAdj=self.liborFreqSpreadAdjC()

        __adj=1/(1+self.defaultRate+self.expense)**(1/self.frequency)
        self.adjVectorCF=self.defaultAdjVectorC(__adj)

        self.adjustedAsset=self.adjCfC()

        self.alm=self.almAdjAssetC()
        self.accumulateCf()

        return self.totalReinvAmt[self.frequency*self.maxyear-1]*1/(1+self.liborFreqSpreadAdj[self.frequency*self.maxyear-1])

    def PercentageAssetC(self,guess) :
        return newton(self.performLossRec,guess)

    def shadowLossC(self,percentage,bvFix,bvDyn,mvFix,mvDyn):
        __bv=percentage*bvDyn+bvFix
        __mv=percentage*mvDyn+mvFix
        return __mv-__bv

class requiredAmount(report):
    def __init__(self):

        #initial inputs

        report.__init__(self)
        self.PhoenixVariableName=('date','phoenixRate')
        self.phoenixRates={}
        self.du=0
        self.mvAdmissible=0
        #static assumptions that should be in MongoDb and queried

        self.usdToGbp=None
        self.usdToEur=None

        #calculated results
        self.portfolioIrr=0
        self.adjustedPortfolioIrr=0


        self.adjustedPhoenixRates={}

        self.mvToPhoenixValuesRatio={}
        self.projectedMv={}
        self.accPhoenix={}
        self.discountRateWeighted={}
        self.discountVectorWeighted={}

        self.requiredAmount=0
        self.collateralReq=0


#calculate the adjusted IRR of the porfolio by deducting default, haircut and expenses
    def adjustedPortfolioIrrC(self):
        _temp=0.0
        _temp=(1+self.portfolioIrr)**(self.frequency)-1
        _temp=(_temp-self.defaultRate)*self.haircut-self.expense

        return (1+(__temp-self.defaultRate)*self.haircut-self.expense)**(1/self.frequency)-1

    #adjust the phoenix rates by deducting haircut default and expenses
    def adjustedPhoenixRatesC(self):
        return {k:(1+(v-self.defaultRate)*self.haircut-self.expense)**(1/self.frequency)-1 for (k,v) in self.phoenixRates.items()}


    #perform asset minus liabilities on adjusted assets
    def almAdjAssetC(self):
        return {k:self.adjustedAsset[k]-v for (k,v) in self.liability.items()}

 #project market value forward in time by using starting asset+cash incremented by the Adjusted IRR minus the asset CF
    def projectedMvC(self):
        __values={}
        __start=self.mv+self.cash
        for n in range(1,len(self.asset)+1):
            if n==1:
                __values[n]=__start*(1+self.adjustedPortfolioIrr)-self.adjustedAsset[n]

            else:
                __values[n]=__values[n-1]*(1+self.adjustedPortfolioIrr)-self.adjustedAsset[n]
        return __values
    #accumulate the asset minus liabilities CF at the adjusted phoenix rates
    def accPhoenixValuesC(self):
        __values={}

        for n in range(1,len(self.alm)+1):
            if n==1:
                __values[n]=self.alm[n]

            else:
                __values[n]=__values[n-1]*(1+self.adjustedPhoenixRates[n])+self.alm[n]

        return __values
  #create a ratio of accumulated Market values to phoenix accumulated values
    def mvToPhoenixValuesRatioC(self):
        return {k:abs(v)/(abs(v)+self.projectedMv[k]) for (k,v) in self.accPhoenix.items()}
   #create a weighted discount rate using the ratio from mvToPhoenixValuesRatioC, adjustedIRR and Adjusted Phoenix

    def discountRatesWeightedC(self):
        __values={}
        for k,v in self.mvToPhoenixValuesRatio.items():
            if k<=36:
                __values[k]=self.adjustedPortfolioIrr
            else:
                 __values[k]=v*self.adjustedPhoenixRates[k]+(1-v)*self.adjustedPortfolioIrr
        return __values

    #create a discount vector from the weighted discount rates of discountRatesWeighted
    def discountVectorWeightedC(self):
        __values={}
        for k,v in self.discountRateWeighted.items():
            if k==1:
                __values[k]=1/(1+v)
            else:
                __values[k]=__values[k-1]*1/(1+v)
        return __values

    def discountLiabilities(self):
        __values={k:v*self.liability[k] for (k,v) in self.discountVectorWeighted.items()}
        return sum(__values.values())

    def collateralReqC(self):
        return self.mvAdmissible+self.cash-self.requiredAmount-self.du
    def requiredAmountCompute(self):
        #step1
        self.portfolioIrr=self.portfolioIrrC()
        #step2
        self.adjustedPortfolioIrr=self.adjustedPortfolioIrrC()
        #step3
        __adj=((1+self.adjustedPortfolioIrr)/(1+self.portfolioIrr))

        self.adjVectorCF=self.defaultAdjVectorC(adjFactor=__adj)
        #step4
        self.adjustedAsset=self.adjCfC()
        self.alm=self.almAdjAssetC()
        #step5
        self.projectedMv=self.projectedMvC()
        #step6
        self.adjustedPhoenixRates=self.adjustedPhoenixRatesC()
        #step7
        self.accPhoenix=self.accPhoenixValuesC()
        #step8
        self.mvToPhoenixValuesRatio=self.mvToPhoenixValuesRatioC()
        #step9
        self.discountRateWeighted=self.discountRatesWeightedC()
        #step10
        self.discountVectorWeighted=self.discountVectorWeightedC()
        #step11
        self.requiredAmount=self.discountLiabilities()
        #step12
        self.collateralReq=self.collateralReqC()

def selectByDate(itemList,valDate,dateKey,itemKey):
#function to select from a presorted list items where date > than report valuation date
    __count=1
    __container={}

    for item in itemList:
        if datetime.strptime(valDate,'%Y-%m-%d')<datetime.strptime(item[dateKey],'%Y-%m-%d'):
            __container[__count]=item[itemKey]
            __count=__count+1
    return __container
