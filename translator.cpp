#include <iostream>
#include <vector>
#include <string>
#include <stack>
#include <map>
#include <cmath>
#include <stdlib.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtx/scalar_multiplication.hpp>
#include <glm/gtx/string_cast.hpp>
#include <glm/gtx/rotate_vector.hpp>

template <class objType,class storedObj>
class priorityStorage{

public:
	std::vector<std::vector<storedObj>> storage;
	//map that connect priorities with data rows in storage
	//priority - vector index
	std::map<unsigned int,unsigned int> storageMap;
	unsigned int max_priority;

	priorityStorage(){
		max_priority=0;
	}

	void push(storedObj data,unsigned int priority){
		//if there is no elements with this priority append map with pair and insert data into storage
		if(storageMap.find(priority)==storageMap.end()){
			storageMap.insert(std::pair<unsigned int,unsigned int>(priority,storage.size()));
			storage.push_back(std::vector<storedObj>({data}));
			max_priority = priority>max_priority?priority:max_priority;
		}else{
			storage[storageMap[priority]].push_back(data);
		}
	}
	void show_storage(){
		for(auto it = storageMap.begin();it!=storageMap.end();it++){
			std::cout<<"Priority - "<<it->first<<"	|Index - "<<it->second<<std::endl;
			for(unsigned int j=0;j<storage[it->second].size();j++)
				std::cout<<storage[it->second][j].fn<<",	";
			std::cout<<std::endl;
		}
	}
};

template <typename aType>
class translator{
private:
	struct atom{
		//if sentence atom corresponds to variable(0 if not either index of variable+1)
		unsigned int var;
		//if sentence atom corresponds to function(empty if not)
		std::string fn;
		//if sentence atom corresponds to value (NULL if not)
		aType* value;
	};
	//resulting function sequence of atoms
	std::vector<atom> funcSequence;
	//variable vocabulary
	std::vector<std::string> varVocab;
	//vocabulary of double argument functions
	std::map<std::string,aType (*)(aType,aType)> doubleArg;
	//vocabulary of single argument functions
	std::map<std::string,aType (*)(aType)> singArg;
	//priorities of operations
	std::map<std::string,unsigned int> priority;

	//user-defined function
	std::string function;

	unsigned int unar_priority;
	int parenthesis;
	//function that reconizes phoneme of language and translates it into atom of new one
	template<class funcType>
	std::pair<std::string,funcType> recognizeFunc(std::map<std::string,funcType> recogIn,std::string toRecog,bool* r=nullptr){
		std::pair<std::string,funcType> ans;
		ans.first="";
		if(r!=nullptr)
			*r = false;
		for(auto it = recogIn.begin();it!=recogIn.end();++it){
			if(toRecog.compare(it->first)==0){
				ans.first=it->first;
				ans.second=it->second;
				if(r!=nullptr)
					*r = true;
				return ans;
			}
		}
		return ans;
	}

	atom atomize(unsigned int* pos,bool* recog,bool* emty)
	{
		atom ans;
		ans.fn="";
		ans.value=nullptr;
		ans.var=0;
		for(*pos;*pos<function.length();(*pos)++){
			//if number found
			if(48<=function[*pos]&&function[*pos]<=57){
				int a=*pos;
				while((48<=function[*pos]&&function[*pos]<=57)|| function[*pos]=='.'||function[*pos]=='e'||((function[*pos]=='-'||function[*pos]=='+')&&function[*pos-1]=='e'))
					(*pos)++;
				aType* value = new aType;
				*value = (aType)std::stod(function.substr(a, *pos-a));
				(*pos)--;
				ans.value=value;
				std::cout<<value<<" recognized"<<std::endl;
				*recog = true;
				return ans;
			}else{
				*recog=false;
			//if possible function found
				for(auto it = doubleArg.begin();it!=doubleArg.end();++it){
					std::string match = recognizeFunc<aType (*)(aType,aType)>(doubleArg, function.substr(*pos,it->first.size()), recog).first;
					if(!match.empty()){
						*pos+=it->first.size()-1;
						ans.fn=match;
						std::cout<<match<<" recognized"<<std::endl;
						return ans;
					}
				}
				for(auto it = singArg.begin();it!=singArg.end();++it){
					std::string match = recognizeFunc<aType (*)(aType)>(singArg, function.substr(*pos,it->first.size()), recog).first;
					if(!match.empty()){
						*pos+=it->first.size()-1;
						ans.fn=match;
						std::cout<<match<<" recognized"<<std::endl;
						//set flag of found function of one argument to true
						*emty=true;
						return ans;
					}
				}
				for(int it = 0;it!=varVocab.size();it++){
					if(function.substr(*pos,varVocab[it].size()).compare(varVocab[it])==0){
						*pos+=varVocab[it].size()-1;
						ans.var=it+1;
						std::cout<<varVocab[it]<<" recognized"<<std::endl;
						*recog = true;
						return ans;
					}
				}
				//if nothing found
				*recog = false;
				return ans;
			}
		}
	}
	//function that builds sequence of actions
	bool buildSeq(unsigned int* beg){
		priorityStorage <std::stack<atom>,atom> operations;
		bool empty_sin;
		unsigned int pos;
		for(pos=*beg;pos<function.size();pos++){
			//if parentheses recognized call build function recursively
			if(function[pos]==')'){
				pos--;
				parenthesis--;
				break;
			}
			if(function[pos]=='('){
				*beg=pos+1;
				parenthesis++;
				if(!buildSeq(beg))
					return false;
				pos=*beg;
			}else{
				bool r;
				atom tmp = atomize(&pos,&r,&empty_sin);
				if(!r)
				{
					std::cout<<"INVALID ATOM FOUND AT INDEX "<<pos<<"!"<<std::endl;
					return false;
				}
				if(!tmp.fn.empty()){
					//delete from storage functions with higher priorities
					for(unsigned int i=operations.max_priority;i>=priority[tmp.fn]+1;i--)
					{
						if(operations.storageMap.find(i)!=operations.storageMap.end()){
							unsigned int row_inx = operations.storageMap[i];
							for(int j=(operations.storage[row_inx].size())-1;j>=0;j--)
								funcSequence.push_back(operations.storage[row_inx][j]);
							operations.storage[row_inx].clear();
						}
					}
					//add recognized function to storage
					operations.push(tmp, priority[tmp.fn]);
				}
				if(tmp.value!=nullptr||tmp.var!=0)
				{
					funcSequence.push_back(tmp);
					if(empty_sin){
						for(int i=operations.storage[operations.storageMap[unar_priority]].size()-1;i>=0;i--)
							funcSequence.push_back(operations.storage[operations.storageMap[unar_priority]][i]);
						operations.storage[operations.storageMap[unar_priority]].clear();
						empty_sin=false;
					}
				}
			}
		}
		//empty function storage from higher priority to lower
		for(int i=operations.max_priority;i>=0;i--){
			if(operations.storageMap.find(i)!=operations.storageMap.end()){
				unsigned int row_inx=operations.storageMap[i];
				for(int j=(operations.storage[row_inx].size())-1;j>=0;j--)
					funcSequence.push_back(operations.storage[row_inx][j]);
				operations.storage[row_inx].clear();
			}
		}
		//if at the end of translation program end up with unclosed|unopened parenthesis
		if((pos==(function.size()-1)||pos==(function.size()-2))&&parenthesis!=0)
		{
			std::cout<<"Invalid number of parenthesis!"<<std::endl;
			return false;
		}
		*beg=pos+1;
		return true;
	}
public:

	translator(std::string usrFunc,//user function
			aType (**binFuncs)(aType,aType),//double argument functions
			std::vector<std::string> binVocab,//pointer to pointers
			aType (**sinFuncs)(aType),//single arg funcs
			std::vector<std::string> sinVocab,//pointer to pointers to single arg funcs
			std::vector<unsigned int> priorityVocab,
			std::vector <std::string> variableVocab,
			bool* flag){
		function = usrFunc;
		//form full function vocabulary based on separated function vocabs
		std::vector<std::string> added(binVocab);
		//note priority of single argument functions
		unar_priority=priorityVocab.size()>added.size()?priorityVocab[added.size()]:0;

		//append list of names with single argument functions
		added.insert(added.end(), sinVocab.data(), sinVocab.data()+sinVocab.size());
		//fill vocabularies with pairs of spelling-function pointer
		for(unsigned int i=0;i<binVocab.size();i++)
			doubleArg.insert(std::pair<std::string,aType (*)(aType,aType)>(binVocab[i],binFuncs[i]));

		for(unsigned int i=0;i<sinVocab.size();i++)
			singArg.insert(std::pair<std::string,aType (*)(aType)>(sinVocab[i],sinFuncs[i]));

		for(unsigned int i=0;i<added.size();i++)
			priority.insert(std::pair<std::string,unsigned int>(added[i],priorityVocab[i]));
		//fill variable vocabulary
		varVocab=variableVocab;
		//show filled maps
		std::cout<<"Following phonemes ready to be recognized:"<<std::endl;
		for(int i =0;i<varVocab.size();i++)
			std::cout<<"Variable	"<<varVocab[i]<<std::endl;

		for(auto it = doubleArg.begin();it!=doubleArg.end();++it)
			std::cout<<it->first<<std::endl;

		for(auto it = singArg.begin();it!=singArg.end();++it)
			std::cout<<it->first<<std::endl;

		std::cout<<"Operation priorities:"<<std::endl;
		for(auto it = priority.begin();it!=priority.end();++it)
			std::cout<<"Operation - "<<it->first<<"	|Priority - "<<it->second<<std::endl;

		std::cout<<"Function processing started:"<<std::endl<<function<<std::endl;
		unsigned int st=0;
		parenthesis=0;
		*flag = buildSeq(&st);
	}

	void showSequence(){
		std::cout<<"Resulting function:"<<std::endl;
		for(unsigned int i=0;i<funcSequence.size();i++)
		{
			std::cout<<"|";
			std::cout<<funcSequence[i].fn;
			std::cout<<(funcSequence[i].var==0?std::string(""):varVocab[funcSequence[i].var-1]);
			std::cout<<(funcSequence[i].value==nullptr?std::string(""):std::to_string(*funcSequence[i].value));
			std::cout<<"|";
		}
	}

	aType getValue(aType* point){
		std::vector<aType> ans;
		for(unsigned int i=0;i<funcSequence.size();i++)
		{
			//if constant value is given it is pushed in stack
			if(funcSequence[i].value!=nullptr)
				ans.push_back(*funcSequence[i].value);
			//if variable name is givet it gets replaced with corresponding value from "point" array
			if(funcSequence[i].var!=0)
				ans.push_back(point[funcSequence[i].var-1]);
			//if function name is specified
			if(!funcSequence[i].fn.empty()){
				std::pair<std::string,aType (*)(aType)> matchS = recognizeFunc<aType (*)(aType)>(singArg,funcSequence[i].fn);
				std::pair<std::string,aType (*)(aType,aType)> matchD = recognizeFunc<aType (*)(aType,aType)>(doubleArg,funcSequence[i].fn);
				//and it is name of single argument function top value of "ans" vector gets replaced
				if(!matchS.first.empty()){
					if(ans.size()<1){
						throw 2;
					}else{
						ans[ans.size()-1]=matchS.second(ans[ans.size()-1]);
					}
				}
				//and it is name of double argument function pre-top value of "ans" vector gets replaced with f(pre-top,top),top value deleted
				if(!matchD.first.empty()){
					//if size of array is less than 2 something wrong with function
					if(ans.size()<2){
						throw 2;
					}else{
						ans[ans.size()-2]=matchD.second(ans[ans.size()-2],ans[ans.size()-1]);
						ans.pop_back();
					}
				}
			}
		}
		//if at the end of all operations single value wasn`t reached throw error
		if(ans.size()!=1)
			throw 1;
		//if at the end of all operations result is infinity throw corresponding code
		if(std::isinf(ans[0]))
			throw 3;
		return ans[0];
	}

	~translator(){
		for(unsigned int i =0;i<funcSequence.size();i++){
			if(funcSequence[i].value!=nullptr)
				delete funcSequence[i].value;
		}
	}
};

float plus(float a,float b){
	return a+b;
}

float minus(float a,float b){
	return a-b;
}

float multi(float a,float b){
	return a*b;
}

float divid(float a,float b){
	return a/b;
}


float sing(float a){
	return glm::sin(glm::radians(a));
}

float cosg(float a){
	return glm::cos(glm::radians(a));
}

float pw(float a,float b){
	return glm::pow(a,b);
}

float sqt(float a){
	return glm::sqrt(a);
}
int main() {
	std::vector <float (*)(float,float)> dubFuncs({*plus,*minus,*multi,*divid,*pw});
	std::vector <float (*)(float)> sinFuncs({*sing,*cosg,*sqt});
	std::vector <std::string> dubVocab({"+","-","*","/","^"});
	std::vector <std::string> sinVocab({"sin","cos","sqrt"});
	std::vector <std::string> varVocab({"x","y"});
	std::vector<unsigned int> priVocab({1,1,2,2,3,4,4,4});
	bool success;
	translator<float> test(std::string("(2.71828^((0-x*x)/2))/(sqrt(2*3.1415926))"),dubFuncs.data(),dubVocab,sinFuncs.data(),sinVocab,priVocab,varVocab,&success);
	std::cout<<(success?"Function built successfully!":"Function built with errors!")<<std::endl;
	test.showSequence();
	float* values = new float[2];
	//folowing values ccan be modified to get different results of given function
	values[0]=-0.9731;
	values[1]=5.0;
	while(1){
		std::cout<<std::endl<<"Enter x:";
		std::cin>>values[0];
		try
		{
			std::cout<<std::endl<<"Resulting Value:"<<test.getValue(values)<<std::endl;
		}catch(int ex_number){
			switch(ex_number)
			{
			case 1:
				std::cout<<"Function doesn`t converge to a single value!"<<std::endl;
				break;
			case 2:
				std::cout<<"Invalid argument number!"<<std::endl;
				break;
			case 3:
				std::cout<<"Function ended up with result of infinity"<<std::endl;
			}
		}
	}
	std::cout<<std::endl<<"Insertin values:"<<std::endl;
	for(unsigned int i=0;i<varVocab.size();i++)
		std::cout<<varVocab[i]<<" - "<<values[i]<<std::endl;
	//exeption hangling
	try
	{
		std::cout<<std::endl<<"Resulting Value:"<<test.getValue(values)<<std::endl;
	}catch(int ex_number){
		switch(ex_number)
		{
		case 1:
			std::cout<<"Function doesn`t converge to a single value!"<<std::endl;
			break;
		case 2:
			std::cout<<"Invalid argument number!"<<std::endl;
			break;
		case 3:
			std::cout<<"Function ended up with result of infinity"<<std::endl;
		}
	}
	delete[] values;
	return 0;
}
