
import QuantLib as ql
import pandas as pd
import numpy as np
from scipy.optimize import newton, root_scalar

#to correct later on with app

def extract_info_from_curve(valDate,originScenCurve,xSpreads,colNames,spotCurve):
    rates=[]
    dates=list(spotCurve.dates())
    
    years=0
    __scenCurve = ql.SpreadedLinearZeroInterpolatedTermStructure(ql.YieldTermStructureHandle(originScenCurve),[ ql.QuoteHandle(q) for q in xSpreads ],dates)
   
    for t in range(0,len(xSpreads)):
        #years=years+1
        date=dates[t]
        #nb years between valuation date and future date
        yearPassed=ql.ActualActual().yearFraction(valDate, date)
        #if yearPassed>97:
            #break
        #append to the list
        rates.append(__scenCurve.zeroRate(yearPassed,ql.Compounded).rate())
    #reutrns a dataframe of date & scenario curve    
    return pd.DataFrame(list(zip(dates, rates)),columns=colNames)

def calibrate_term_structure(baseSpotCurve,listSpreads,startDate,name):
    dates=list(baseSpotCurve.dates())
    spreads = [ ql.SimpleQuote(0.0) for n in dates ] # null spreads to begin
    scenCurve = ql.SpreadedLinearZeroInterpolatedTermStructure(ql.YieldTermStructureHandle(baseSpotCurve),[ql.QuoteHandle(q) for q in spreads],dates)
    
   
    colname=name
    for run in range(1,3):
        for t in range(1,100):
            if t>35:

            #target=spreads[t].value()
                target=listSpreads[-1]
            else:
                target=listSpreads[t]    
            #print(target)
            timePeriod=dates[t-1]
            myGuess=target*1.05
            newton(forward_match,myGuess,args=(target,startDate,timePeriod,spreads,scenCurve,baseSpotCurve,t))
    return scenCurve        

def extract_info(curve,dates,valDate,name='spot_rate'):
    rates=[]
    for t in range(0,len(dates)):
        #years=years+1
        date=dates[t]
        #nb years between valuation date and future date
        yearPassed=ql.ActualActual().yearFraction(valDate, date)

        rates.append(curve.zeroRate(yearPassed,ql.Compounded).rate())
    #reutrns a dataframe of date & scenario curve    
    return pd.DataFrame(list(zip(dates, rates)),columns=['date',name])


def spot_rate_match(guess,ScenCurve,baseCurve,todayDate,spotDate,spreads,target,indx):
    
    #futureDate=ql.TARGET().advance(todaysDate,nbfutureYears,Years)
    futureDate=spotDate
    baseCurveHandle = ql.YieldTermStructureHandle(baseCurve)
    
    spreads[indx].setValue(guess)
    yearPassed=ql.ActualActual().yearFraction(todayDate, spotDate)
    #yearPassed=indx
    
    scenHandle=ql.YieldTermStructureHandle(ScenCurve)
    scenTarget=scenHandle.zeroRate(yearPassed,ql.Compounded).rate()
    baseTarget=baseCurveHandle.zeroRate(yearPassed,ql.Compounded).rate()
    return scenTarget-baseTarget-target
    

    
def forward_match(guess,target,todayDate,fDate,spreads,ScenCurve,baseCurve,nbfutureYears):
    #print('X')
    #futureDate=ql.TARGET().advance(todaysDate,nbfutureYears,Years)
    #futureDate=fDate
    baseCurveHandle = ql.YieldTermStructureHandle(baseCurve)
    
    spreads[nbfutureYears].setValue(guess)
    
    baseImpl=ql.ImpliedTermStructure(baseCurveHandle,fDate)
    #always the 1 year forward from a given future date calculated using the TARGET()
    baseZeroRate=baseImpl.zeroRate(1,ql.Compounded).rate()
    #print(baseZeroRate)
    scenHandle=ql.YieldTermStructureHandle(ScenCurve)
    
    scenImpl=ql.ImpliedTermStructure(scenHandle,fDate)
    scenZeroRate=scenImpl.zeroRate(1,ql.Compounded).rate()
    #print(scenZeroRate)
    #print(spreads[nbfutureYears].value())
    #print(scenZeroRate-baseZeroRate-target)
    return (scenZeroRate-baseZeroRate-target)