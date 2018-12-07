import numpy as np
import random as rd
from copy import copy, deepcopy
from collections import namedtuple, deque
import pandas as pd
#sigmoid function for signal dampening
def sigmoid(x):
    return 1/(1+np.exp(-x))

#this class generates claim costs
class ClaimCost:
    def __init__(self):
        #based on products specs those appears to be the values for low, expected and high
        self.low=0
        self.exp=2
        self.high=3
        self.medInfLow=0.045
        self.medInfMed=0.06
        self.medInfHigh=0.09
        
        self.medInflation=False
        self.proportionInflated=1
        #binary is for a catastrophic claim possibility
        self.binary=False
        self.isHighClaimer=0.15
        self.multipleforHighClaimer=2
    def calculateExpectedCost(self):
        #Since it is not possible to truly know the distribution of outcomes
        #rd.triangular assign higher probabilty to values above mode than below something like a beta dis
        if self.binary==True:
            if rd.random()<self.isHighClaimer:
                return -rd.triangular(low=self.low,mode=self.exp,high=self.high)*self.multipleforHighClaimer
            else:
                return -rd.triangular(low=self.low,mode=self.exp,high=self.high)   
        else:
            return -rd.triangular(low=self.low,mode=self.exp,high=self.high)
        
    def calcMedInfAdj(self,time):
        __infValue=(1+rd.triangular(low=self.medInfLow,mode=self.medInfMed,high=self.medInfHigh))**time
        __weightedInf=self.proportionInflated*__infValue+(1-self.proportionInflated)
        return __weightedInf
    
#this class generates a human living.
class Human:

    def __init__(self, initQx=0.023,initAge=70):
        #Gender set to male but then randomly assigned
        self.male=True
        self.age=initAge
        #initial Qx is set to 0.023 but can be changed at will
        self.initialQx=initQx
        #doubling rate of mortality like in Makeham's formula
        self.double=0.090
        #max Qx
        self.maxQx=0.45
        #Standard from SCOR as base value as default
        self.cancerOdds=0.4
        #of course you don't start with cancer
        self.cancer=False
        #dictionary to keep mortality curve created
        self.oddsOfDying={}
        self.actuarialTable={}
        #boolean for death
        self.isDead=False
        #time since simulation started
        self.time=0
        #set to not married at first
        self.married=False
        #odds of being married are conservatively less than true percentage of population
        self.oddsOfBeingMarried=0.2
        #boolean to assess when/if you get a curable cancer
        self.hasSurvivedCancer=False

    #given that the policy dies now decides if it is because of cancer
    def assessCancer(self):
        #if random is less than threshold then cancer=true
        if rd.random()<=self.cancerOdds:
            
            self.cancer=True     
                    
#ok you may get a curable cancer over a life time but let's hope it is less than 1/3 the Qx otherwise everyone gets it so it is floored at 1 in 400 or 0.0025
    def assessIfWillSufferCurableCancer(self):
        if self.hasSurvivedCancer==False:
            if rd.random()<=min(self.oddsOfDying[self.time]/4,0.0025):
            #if rd.random()<=0.002:
                self.hasSurvivedCancer=True
        else:
            pass
# a proxy function for when policy is transfred to the spouse.        
    def performSexChange(self):
        #your spouse is a bit younger so you are rejuvinated a bit
        if self.male==True:
            self.time=self.time-3
            #you are widowed now
            self.married=False
            #female have base Qx about 30% less than male (hard coded 0.7 is messy find something better!!)
            self.initialQx = self.initialQx*0.7
            #you are a woman now! of course there is also the unlikely possiblity of a same sex couple but hey!
            self.Male=False
        # if you are a female then your spouse is a male
        else:
            self.time=self.time+3
            #you are widowed now
            self.married=False
            #male have base Qx about approx 40% higher than males
            self.initialQx = self.initialQx*1.4
            #you are a woman now! of course there is also the unlikely possiblity of a same sex couple but hey!
            self.Male=False
            
     #to decide at every time step if you die 
    def assessDeath(self):
        #If random is less than treshold then you die
        if rd.random()< self.oddsOfDying[self.time]:
            self.isDead=True

     # if a table is not used then it is time 0 qx that increase at 9%/year it is close enough   
    def calculateOddsofDying(self):
        #start with baseQx of time 0 and grow accroding to doubling rate it is a crude approximation of the Makeham formula but it is nonetheless very close
        __qx=self.initialQx*(1+self.double)**self.time
        return min(__qx,self.maxQx)

    #to pick up values from the Act Qx table
    def qxActTable(self):
        currentAge=self.age+self.time
        if self.male==True:
            
            __qx=self.actuarialTable['Male Death'][currentAge]
        else:
            __qx=self.actuarialTable['Female Death'][currentAge]
        return min(__qx,self.maxQx)

    def marryMe(self):
        #randomly assign marital status
        if rd.random()<self.oddsOfBeingMarried:
            self.married=True
            
    def calibrateStartQxForGender(self):
        if self.male==False:
            #woman mortality is always about 30% lower until extreme old age (105+)
            self.initialQx=0.7*self.initialQx

    def assessIfSpouseAlive(self):
        # conservative estimate of the odds of the spouse being alive if transfered. doesn't really have to be precise but we could :)
        if self.married==True:
            if self.male==True:
                if rd.randmom()<0.5:
                    return True
                else:
                    return False
            #you are a female and odds are your are a widow already
            else:
                if rd.random()<0.33:
                    return True
                else:
                    return False
        else:
            return False
   # reset all parameters for next simulation     
    def reset(self,gender,maritalStatus):
        self.male=gender
        self.oddsOfDying.clear()
        self.isDead=False
        self.married=maritalStatus
        self.time=0
        self.cancer=False
        self.hasSurvivedCancer=False
        self.cancerOdds=0.4

class Cancer:
    #this class assign cancer odds based on a low med high ends of probability
    def __init__(self):
        self.lowEnd=0.0
        self.Mode=0.0
        self.high=0.0
        self.femaleAddOn=0.05
        self.durationUntilDeath=2
        self.incidenceTable=dict()
    def assignCancerOdds(self,gender):
            #low medium and high bounds for cancer odds based on SCOR studies
            #True= Male
            if gender==True:
                return rd.triangular(low=self.lowEnd,mode=self.Mode,high=self.high)
            else:
                #woman have slighly higher odds since they are less prone to die from everything else but still die like everyone else
                
                return rd.triangular(low=self.lowEnd,mode=self.Mode,high=self.high)+self.femaleAddOn
            
    def assignCancerDuration(self):
        return rd.triangular(low=1,mode=self.durationUntilDeath,high=4)

class Policy:
#a policy class contains a human class, a claim cost class and a cancer class and a premium schedule
    def __init__(self,policyHolder,claimCost,Cancer):
        self.policyHolder=policyHolder
        self.productLine='Initech'
        self.claimCost=claimCost
        self.cancer=Cancer
        self.premium=0
        self.actualClaim=0
        self.paidCurableCancerClaim=False  
        self.curableCancerClaim=0
        self.maturity=100
        self.maxLapse=0
        self.lapseCurrent=0
    def reset(self):
        self.paidCurableCancerClaim=False
        self.curableCancerClaim=0
        self.actualClaim=0
        self.lapseCurrent=0
        
    def payPremium(self):
        return self.premium

#the critic function tries to assess the value of his policy, the more the premiums exceed the death benefit, the more signal the function outputs
# this is a relu function. once the ratio of premium to benefits exceeds 2 the signal stops
#if the ratio is less than 0 then there is no signal
    def euristicCritic(self):
        #function uses a low discount close to inflation
        __discount=0.02
        # people assume they make it to 85
        __assumedAgeDeath=85
    #when making the assessment, if the ratio of premiums/benefits is <1.0 there is no signal
        _threshold=1.0
        __currentAge=self.policyHolder.age+self.policyHolder.time
        #life expectancy for a person is simply 85 minus current age
        __lifeExp=max(1,__assumedAgeDeath-__currentAge)
        #Pv of premiyums
        __pvPremiums=np.npv(__discount,np.full(__lifeExp,self.premium))
        #pv of benefits
        __pvBenefit=0.55*self.claimCost.exp/(1+__discount)**__lifeExp
        
        #signal relu function
        #the signal is the ratio of premiums paid to benefits
        if __pvPremiums/__pvBenefit<=_threshold:
            __value=0
        else:
            #add some white noise since people are capable of assessing the value of these things but they mess up a bit 
            __whiteNoise=rd.gauss(-0.05,0.05)
            #__whiteNoise=0
            __value=min(max(__whiteNoise+__pvPremiums/__pvBenefit-_threshold,0),1)
        #print(__value)
        return __value
        
    def lapsingCurrentPeriod(self):
       #lapse is maximal rate times the critic's signal which is between 0 and 1     
        __currentLapse=self.maxLapse*self.euristicCritic()
        # simulate lapse
        if rd.random()<__currentLapse:
            return True
        else:
            return False
    def cancerCosts(self):
        #if Cancer then simulate an expected cost 
        if self.policyHolder.cancer==True:
            __claim=self.claimCost.calculateExpectedCost()
                
            #verify you don't have to add medical inflation
            if self.claimCost.medInflation==False:
                pass
            else:
            #add random medical inflation
                __claim=claim__*self.claimCost.calcMedInfAdj(self.policyHolder.time)  
            #premium is waived when you die from cancer and from the moment you got diagnosed                
            __premium=-self.premium*self.cancer.assignCancerDuration()
            
        else:
            __claim=0
            __premium=self.premium
            
        return __premium,__claim
#gp:this function is too big is should be broken into more more methods!    

    def timestepSoa(self):
        #move forward in time
        self.policyHolder.time+=1
        #refresh cancer odds
        __currentAge=self.policyHolder.time+self.policyHolder.age
        self.policyholder.cancerOdds=self.cancer.incidenceTable[(self.policyHolder.male,__currentAge)]
        #calculate odds of dying during current period
        #oddsOfDying=self.policyHolder.calculateOddsofDying()
        oddsOfDying=self.policyHolder.qxActTable()
        #add this info ot the dictionary of the human
        self.policyHolder.oddsOfDying[self.policyHolder.time]=oddsOfDying
        #assess if dead
        self.policyHolder.assessDeath()
        #assess cancer incidence
        self.policyHolder.assessCancer()
        #calculateCancerCosts
        premium,self.currentClaim=self.cancerCosts()            
        
        if self.policyHolder.isDead==True:
            done=True
        else:
            done=False
            return self.actualClaim,premium,done
            
    def timestep(self):
        #move forward in time
        self.policyHolder.time+=1
        #calculate odds of dying during current period
        #oddsOfDying=self.policyHolder.calculateOddsofDying()
        oddsOfDying=self.policyHolder.qxActTable()
        #add this info ot the dictionary of the human
        self.policyHolder.oddsOfDying[self.policyHolder.time]=oddsOfDying
        
        #assess if dead
        self.policyHolder.assessDeath()

        #if death then assess if death is due to Cancer
        if self.policyHolder.isDead==True:
            self.policyHolder.assessCancer()

            #if both dead AND from Cancer then simulate an expected cost 
            if self.policyHolder.cancer==True:
                self.actualClaim=self.claimCost.calculateExpectedCost()
                
                #verify you don't have to add medical inflation
                if self.claimCost.medInflation==False:
                    pass
                else:
                    #add random medical inflation
                    self.actualClaim=self.actualClaim*self.claimCost.calcMedInfAdj(self.policyHolder.time)  
                #premium is waived when you die from cancer and from the moment you got diagnosed
                
                premium=-self.premium*self.cancer.assignCancerDuration()
                #premium=0
                done=True
                return self.actualClaim,premium,done
            #if you are dead but not from Cancer you are out but you cost 0
            else:
                self.actualClaim=0
                premium=self.payPremium()
                done=True
                # but wait a minute, if you are married you can transfer the policy to wifey!
                #first verify the spouse is alive:
                _doa=self.policyHolder.assessIfSpouseAlive()
                if self.policyHolder.married==True and _doa==True:
                    #you married someone younger than you
                    self.policyHolder.performSexChange()
                    #well we're not done yet!
                    done=False
                    return self.actualClaim,premium,done
                else:
                    return self.actualClaim,premium,done
        
        #verify that you haven't got cancer but a curable one(if such thing exists)
        else:
            self.policyHolder.assessIfWillSufferCurableCancer()
            if self.policyHolder.hasSurvivedCancer==True and self.paidCurableCancerClaim==False:
                self.curableCancerClaim=0.5*self.claimCost.calculateExpectedCost()
                premium=-0.5*self.premium*self.cancer.assignCancerDuration()
                done=False
                self.paidCurableCancerClaim=True
                return self.curableCancerClaim,premium,done
            #if alive and not cancer plagged carry on to next time step
            else:    
                premium=self.payPremium()
                treatmentCost=0
                done=self.lapsingCurrentPeriod()
                #done=False
                return self.actualClaim,premium,done

    def simulation(self,nbsim,discount=0.040):
        # a simulation moves a policy step by step through time and accumulates a PV of premiums and costs over time.
        #default discount is flat 4%t
        __results={}
        __time=0
        #cancerOdds=[]
        Qx=deepcopy(self.policyHolder.initialQx)
        isMarried=deepcopy(self.policyHolder.married)
        gender=deepcopy(self.policyHolder.male)
        for sim in range(nbsim):
            done=False
            netcost=0
            self.policyHolder.cancerOdds=self.cancer.assignCancerOdds(self.policyHolder.male)
            
            #cancerOdds.append(deepcopy(self.policyHolder.cancerOdds))
            while done !=True and (self.policyHolder.time+self.policyHolder.age)<115:
                #move in time by 1 time step and return cost, premium and done=true/false
                costs,premium,done=self.timestep()
                #print('T=:',self.policyHolder.time,'Cost: ',costs,'Premium: ',premium, 'Qx:',self.policyHolder.oddsOfDying[self.policyHolder.time])
                #creates a present value of costs+ premiums
                netcost+= costs*(1+discount)**(-self.policyHolder.time)
                netcost+=premium*(1+discount)**(-self.policyHolder.time)
                netcost=netcost
            __time+=self.policyHolder.time
            
            __results[sim]=netcost
            self.policyHolder.reset(gender=gender,maritalStatus=isMarried)
            self.policyHolder.initialQx=Qx
            self.policyHolder.married=isMarried
            self.reset()
        return sum(__results.values())/nbsim,__time/nbsim




#class consisting of a named tuple to store information 
class Information:
    def __init__(self):
        """Initialize a ReplayBuffer object.
        Params
        ======
            buffer_size (int): maximum size of buffer
            batch_size (int): size of each training batch
        """
      
        self.memory = list()  # internal memory list
        self.infos=list()      
        self.experience = namedtuple("KeyValues", field_names=["productLine","age","gender","married","claimCost", "timePeriod"])
        self.simInf=namedtuple("Objects", field_names=["policy","plaims","humans"])
    
    def add(self, claimCost, age, gender, married,productLine,timePeriod):
        """Add a new experience to memory."""
        e = self.experience(productLine,age,gender,married,claimCost,timePeriod)
        self.memory.append(e)
    def infoAdd(self, policy,claims,humans):
        h=self.simInf(policy,claims,humans)
        self.infos.append(h)
        
def assignGender():
    #hard coded for now this block is 2/3 woman in every LOB
    if rd.random()<=0.65:
        return True
    else:
        return False

def QxPerAge(baseQx,doublingRate,time,maxQx):
    #function that assigns a starting Qx when running simulations.Based on Makeham's formula
    __qx=baseQx*(1+doublingRate)**time
    return min(__qx,maxQx)
    

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 
if __name__ == "__main__":
    import json

    
#get social security table in a dictionary:
    spreadsheet=pd.ExcelFile(r'C:\dev to W drive\Cancer data analytics\Us Social security.xlsx',dtype='float')
    df = spreadsheet.parse('Social Security')
    df.set_index("exact age", drop=True, inplace=True)
    actTable = df[['Male Death','Female Death']].to_dict(orient="dict")
# get premiums
    prmDf=pd.read_csv(r'C:\dev to W drive\Cancer data analytics\averagePremiumPerPlanCancer.csv')
    
# create a human
    humans={}
    #create a collection of humans.
    #gp: finish it later so it is a nested dictionary with gender and marital status to simulate all possibilities
    for sex in range(1,3):
        for age in range(2,96):
                
            billLumberg=Human()
            billLumberg.actuarialTable=actTable
            billLumberg.age=age
            billLumberg.initialQx=billLumberg.qxActTable()
            billLumberg.double=0.09
            billLumberg.married=False
            if sex==1:
                billLumberg.male=True
            else:
                billLumberg.male=False
            humans[(billLumberg.male,age)]=billLumberg
            
    #characteristic for randomization of humans
    #billLumberg.oddsOfBeingMarried=0.2
    #will also randomize gender if necessary
    
# create a claim
    claim1=ClaimCost()
    claimbump=0
    #low medium and high range of severiy of claims
    claim1.low=20000+claimbump
    claim1.exp=21000+claimbump
    claim1.high=22000+claimbump
    #low medium and high range of medical inflation if necessary
    claim1.medInfLow=0.06
    claim1.medInfMed=0.08
    claim1.medInfHigh=0.09
    claim1.medInflation=False
    claim1.binary=True
    #proportion of claims subject to medical inflation
    claim1.proportionInflated=0.6
    #probability that a person will be a catastrophic claim 
    claim1.isHighClaimer=0.01
    #extent by which it is higher than the average claim
    claim1.multipleforHighClaimer=1.5

#create a Cancer object    
    cancerbump=0.0
    emperorOfMaladies=Cancer()
    emperorOfMaladies.lowEnd=0.40+cancerbump
    emperorOfMaladies.Mode=0.43+cancerbump
    emperorOfMaladies.high=0.45+cancerbump
    emperorOfMaladies.femaleAddOn=0.05
#policy premium and number of simulations to perform
    premium=80
    nbsim=20000
    
# create a policy
    results=Information()


    for key, value in humans.items():
        AIG=Policy(humans[key],claim1,emperorOfMaladies)
        
        #AIG.premium=80
        AIG.productLine='2003 CANCER'
        #the max lapse is defined as given a policy reaches a level where pv of premiums is 2x pv benefits, at least 1 in N person will figure it out in any given year
        #lapse is therefore 1/N
        AIG.maxLapse=0.125

        #picking up the right premium
        if AIG.policyHolder.male==True:
            sex='M'
        else:
            sex='F'
        #to pick up average premium for a given age,product line and gender
        try:
            AIG.premium=prmDf[(prmDf.Type_Of_Policy == AIG.productLine) & (prmDf.CurrentAge== key[1])&(prmDf.Gender_==sex)].Premium.item()
        except:
            AIG.premium=0
        print(AIG.premium)
        
        #simulation Results
        expectedValue,LE=AIG.simulation(nbsim)
        
        #key values to store
        _claimCost= expectedValue
        _age=AIG.policyHolder.age
        _gender=AIG.policyHolder.male
        _married=AIG.policyHolder.married
        _productLine=AIG.productLine
        _timePeriod=LE
        
        #append results to the information Object
        results.add(productLine=_productLine,age=_age,gender=_gender, married=_married,claimCost=_claimCost, timePeriod=LE)
        
    #print(results.memory)
    #transform the claims and Policy objects into Dictionaries
    claimsDict=AIG.claimCost.__dict__
    policyDict=AIG.__dict__
    cancerDict=AIG.cancer.__dict__
    policyDict['claimCost']=claimsDict
    policyDict['cancer']=cancerDict
    policyDict['policyHolder']=results.memory
    
    #print(policyDict)    
    # to transform the named tuple back into a dataframe

    with open(r'C:\dev to W drive\Cancer data analytics\2003 CANCER.json', 'w') as fp:
        json.dump(policyDict, fp)
#696-9986

    

          
            
        
        

            
        
        
