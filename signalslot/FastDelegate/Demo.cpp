#include <stdio.h>
#include "FastDelegate.h"
// Demonstrate the syntax for FastDelegates.
//				-Don Clugston, May 2004.
// It's a really boring example, but it shows the most important cases.

// Declare some functions of varying complexity...
void SimpleStaticFunction(int num, char *str) {
	printf("In SimpleStaticFunction. Num=%d, str = %s\n", num, str);
}

void SimpleVoidFunction() {
	printf("In SimpleVoidFunction with no parameters.\n");
}

class CBaseClass {
protected:
	char *m_name;
public:
	CBaseClass(char *name) : m_name(name) {};
	void SimpleMemberFunction(int num, char *str) {
		printf("In SimpleMemberFunction in %s. Num=%d, str = %s\n", m_name, num, str);	}
	int SimpleMemberFunctionReturnsInt(int num, char *str) {
		printf("In SimpleMemberFunction in %s. Num=%d, str = %s\n", m_name, num, str); return -1;	}
	void ConstMemberFunction(int num, char *str) const {
		printf("In ConstMemberFunction in %s. Num=%d, str = %s\n", m_name, num, str);	}
	virtual void SimpleVirtualFunction(int num, char *str) {
		printf("In SimpleVirtualFunction in %s. Num=%d, str = %s\n", m_name, num, str);	}
	static void StaticMemberFunction(int num, char *str) {
		printf("In StaticMemberFunction. Num=%d, str =%s\n", num, str);	}
};

class COtherClass {
	double rubbish; // to ensure this class has non-zero size.
public:
	virtual void UnusedVirtualFunction(void) { }
	virtual void TrickyVirtualFunction(int num, char *str)=0;
};

class VeryBigClass {
	int letsMakeThingsComplicated[400];
};

// This declaration ensures that we get a convoluted class heirarchy.
class CDerivedClass : public VeryBigClass, virtual public COtherClass, virtual public CBaseClass
{
	double m_somemember[8];
public:
	CDerivedClass() : CBaseClass("Base of Derived") { m_somemember[0]=1.2345; }
	void SimpleDerivedFunction(int num, char *str) { printf("In SimpleDerived. num=%d\n", num); }
	virtual void AnotherUnusedVirtualFunction(int num, char *str) {}
	virtual void TrickyVirtualFunction(int num, char *str) {
		printf("In Derived TrickyMemberFunction. Num=%d, str = %s\n", num, str);
	}
};

using namespace fastdelegate;

int main(void)
{
	// Delegates with up to 8 parameters are supported.
	// Here's the case for a void function.
	// We declare a delegate and attach it to SimpleVoidFunction()
	printf("-- FastDelegate demo --\nA no-parameter delegate is declared using FastDelegate0\n\n");

	FastDelegate0<> noparameterdelegate(&SimpleVoidFunction);

	noparameterdelegate(); // invoke the delegate - this calls SimpleVoidFunction()

	printf("\n-- Examples using two-parameter delegates (int, char *) --\n\n");

    // By default, the return value is void.
    typedef FastDelegate2<int, char *> MyDelegate;

	// If you want to have a non-void return value, put it at the end.
    typedef FastDelegate2<int, char *, int> IntMyDelegate;


	MyDelegate funclist[12]; // delegates are initialized to empty
	CBaseClass a("Base A");
	CBaseClass b("Base B");
	CDerivedClass d;
	CDerivedClass c;

	IntMyDelegate newdeleg;
    newdeleg = MakeDelegate(&a, &CBaseClass::SimpleMemberFunctionReturnsInt);
    
	// Binding a simple member function
        funclist[0].bind(&a, &CBaseClass::SimpleMemberFunction);
		
	// You can also bind static (free) functions
        funclist[1].bind(&SimpleStaticFunction);
	// and static member functions
        funclist[2].bind(&CBaseClass::StaticMemberFunction);
	// and const member functions (these only need a const class pointer).		 
        funclist[11].bind( (const CBaseClass *)&a, &CBaseClass::ConstMemberFunction);
        funclist[3].bind( &a, &CBaseClass::ConstMemberFunction);
	// and virtual member functions
        funclist[4].bind(&b, &CBaseClass::SimpleVirtualFunction);

	// You can also use the = operator. For static functions, a fastdelegate
	// looks identical to a simple function pointer.
        funclist[5] = &CBaseClass::StaticMemberFunction;

	// The weird rule about the class of derived member function pointers is avoided.
	// For MSVC, you can use &CDerivedClass::SimpleVirtualFunction here, but DMC will complain.
	// Note that as well as .bind(), you can also use the MakeDelegate()
	// global function.
        funclist[6] = MakeDelegate(&d, &CBaseClass::SimpleVirtualFunction);

	// The worst case is an abstract virtual function of a virtually-derived class
	// with at least one non-virtual base class. This is a VERY obscure situation,
	// which you're unlikely to encounter in the real world.
	// FastDelegate versions prior to 1.3 had problems with this case on VC6.
	// Now, it works without problems on all compilers.
       funclist[7].bind(&c, &CDerivedClass::TrickyVirtualFunction);
	// BUT... in such cases you should be using the base class as an 
	// interface, anyway.
       funclist[8].bind(&c, &COtherClass::TrickyVirtualFunction);
	// Calling a function that was first declared in the derived class is straightforward
        funclist[9] = MakeDelegate(&c, &CDerivedClass::SimpleDerivedFunction);

	// You can also bind directly using the constructor
        MyDelegate dg(&b, &CBaseClass::SimpleVirtualFunction);

	char *msg = "Looking for equal delegate";
	for (int i=0; i<12; i++) {
		printf("%d :", i);
		// The == and != operators are provided
		// Note that they work even for inline functions.
		if (funclist[i]==dg) { msg = "Found equal delegate"; };
		// operator ! can be used to test for an empty delegate
		// You can also use the .empty() member function.
		if (!funclist[i]) {
			printf("Delegate is empty\n");
		} else {
			// Invocation generates optimal assembly code.
			funclist[i](i, msg);
		};
	}
	return 0;
}

