
import QuantLib as ql
import pandas as pd
import numpy as np
from scipy.optimize import newton, root_scalar
from dataclasses import dataclass,field
from typing import List


#to correct later on with app
#name convention for variables,functions and classes
#Since functions and classes depend largely on QuantLib, the same naming convention has been applied
#for functions: my_function --> all lower cases with underscore
#variable: myVariable --> Pascal Case
#Classes: MyClassNameIs-->Camel Case


#STRUCTS
#close equivalent of a Struct. essentially, a class with no methods and defaults parameters used to share common informations used across classes and functions.


@dataclass
class ActuarialModel:
    """
    DESC:
    a structure that contains key information on actuarial model outputs such as:
    valuation date, the frequency of cash flows(monthly quaterly other)
    the string format of what is used to measure time and the Model' origin
    Some out of the box software have predefined structure so it helps to know from which
    one it is coming from
    
    
    """
    valuationDate:ql.Date=ql.Date(30, 9, 2019)
    fiscalYearEnding:ql.Date=ql.Date(31, 12, 2018)
    engine:str="AXIS"
    cie:str='Evil Re'
    freqency:str='Q'
    discountRate:float=0.04    
    timeFormat:str='SN'
    id:int=112233
    outputs:List=field(default_factory=lambda: [])
    results:List=field(default_factory=lambda: [])

@dataclass
class Results:
    """
    DESC:
    a structure to contain key information on results of a model. it stores the result of calculation using a given metric
    a result has a unique key and stores the key of its parent model and keep it as a foreign key
    """
    id:int=333333
    model_Key:int=112233
    output_Key:int=666
    key:str='aaa??bbb??cc'
    metric:str='npv'
    value:float=0
    ingredients:List=field(default_factory=lambda: [])
        
@dataclass
class DateTimeStruct:
    """
    DESC:
    key inputs for managing dates & time with Quantlib. Contained in a struc it avoids tracking too many inputs
    Only the effective date needs to be changed
    
    """
    effectiveDate:ql.Date = ql.Date(30, 9, 2019)
    terminationDate = ql.Date(30, 9, 2118)
    tenor = ql.Period(ql.Annual)
    calendar = ql.UnitedStates()
    businessConvention = ql.Following
    terminationBusinessConvention = ql.Following
    dateGeneration = ql.DateGeneration.Forward
    endOfMonth = True

@dataclass
class IntRatesStruct:
    """
   DESC:
   key inputs for managing interest rate yield curces with Quantlib. Contained in a struc it 
   avoids tracking too many inputs
    
    """
    interpolation = ql.Linear()
    compounding = ql.Compounded
    compoundingFrequency = ql.Annual
    dayCount=ql.ActualActual()
    isFlat=True
    
def create_schedule(datetimeStruct):
    """
    DESC:
    creates a date schedule. A necessary object for later creating discount curves
    INPUT:
    a DateTimeScruct
    OUTPUT:
    a QuantLib schedule object
    """
    __effectiveDate = datetimeStruct.effectiveDate
    __terminationDate = datetimeStruct.terminationDate
    __tenor = datetimeStruct.tenor
    __calendar = datetimeStruct.calendar
    __businessConvention = datetimeStruct.businessConvention
    __terminationBusinessConvention = datetimeStruct.terminationBusinessConvention
    __dateGeneration = datetimeStruct.dateGeneration
    __endOfMonth = datetimeStruct.endOfMonth
    #creating the schedule
    schedule = ql.Schedule(__effectiveDate,
                             __terminationDate,
                             __tenor,
                             __calendar,
                             __businessConvention,__terminationBusinessConvention,
                             __dateGeneration,
                             __endOfMonth)
    
    return schedule


def create_spotCurve(spotValues,schedule,datetimeStruct,intRateStruct):
    """
    DESC:
    creates a spot curve
    INPUTS:
    spotValues:list=list of spot rates  based on annual time for varying duration
    schedule:Quantlib.Schedule=a schedule object for managing time and days convention
    dateTimeStruct:DatetimeStruct= to pass on inputs related to time
    intRateStruct:IntRatesStruct=to pass on inputs related to compounding methodology
    OUTPUT
    a Quantlib.ZeroCurve
    """
    
    
    __calendar = datetimeStruct.calendar
    __interpolation = intRateStruct.interpolation
    __compounding = intRateStruct.compounding
    __compoundingFrequency = intRateStruct.compoundingFrequency
    __dayCount=intRateStruct.dayCount
    spotCurve = ql.ZeroCurve(spotDates, spotValues, __dayCount, __calendar, __interpolation,__compounding, __compoundingFrequency)
    spotCurveHandle = ql.YieldTermStructureHandle(spotCurve)
    return spotCurve

#create curve
def create_discount_curve(actModelStruct,rates,intRateStruct):
    """
    DESC:
    create a discount curve for cash flows
    INPUTS:
    actModelStruct:ActuarialOutput Data structure 
    rates: either a float if its a flat curve discount or a QuantLib.ZeroCurve object if not
    intRateStruct: interest rate structure to get day count
    OUTPUT
    discount_curve:a QuantLib.YieldTermStructureHandle
    """
    
    if intRateStruct.isFlat==True:
        discount_curve=ql.YieldTermStructureHandle(ql.FlatForward(actModelStruct.valuationDate, rates, intRateStruct.dayCount))
    else:
        if type(rates)!=ql.QuantLib.YieldTermStructureHandle:
            pass
        discount_curve=ql.YieldTermStructureHandle(rates)
    return discount_curve



def period_multiplier(freq):
    """
    DESC: 
    transform a string input representing a time period such as monthly quaterly annually into an integer
    
    """
    if freq.lower()=='q':
        return 3
    elif freq.lower()=='m':
        return 1
    elif freq.lower()=='a':
        return 12
    
def valuation_formula(valDate,freq,elapsedTime):
    """
    DESC:
    creates a quantlib date object
    INPUTS:
    valDate:Date=the starting valuation date
    freq:str= a string defining the metric of time m->monthly q->quaterly a->annual none->annual
    elapsedTime:int=integer reprenting a time period 
    OUTPUT:
    Quantlib date object at last day of the month
    """
    
    periodMultiplier=period_multiplier(freq)
    per=ql.Period(periodMultiplier*elapsedTime,ql.Months)
    return ql.Date.endOfMonth(valDate+per)
        

def time_period_to_real_time(actOutput,period):
    """
    DESC:
    tranform a time period expressed as a string  into a quantlib date using the starting point 
    valuation period of the ActuarialOutput structure
    
    INPUTS:
    actOutput:an ActuarialOutput structure
    period: as string that can have the following format;
    SN=first string is a letter such as Q,A,M and the remainder is an integer
    N: only integers
    
    OUTPUT: QuantLib.Date object
    """

    if actOutput.timeFormat.lower()=='sn':
        freq=period[0]
        elapsedTime=int(period[1:])
        return valuation_formula(actOutput.valuationDate,freq,elapsedTime)
        
    elif actOutput.timeFormat.lower()=='n':
        elapsedTime=int(period)
        return valuation_formula(actOutput.valuationDate,actOutput.timeFormat,elapsedTime)    

    

def calibrate_term_structure(baseSpotCurve,listSpreads,startDate):
    dates=list(baseSpotCurve.dates())
    spreads = [ ql.SimpleQuote(0.0) for n in dates ] # null spreads to begin
    scenCurve = ql.SpreadedLinearZeroInterpolatedTermStructure(ql.YieldTermStructureHandle(baseSpotCurve),[ql.QuoteHandle(q) for q in spreads],dates)
    
   

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