#pragma once
#include "Shape.h"

class Sphere : public Shape{
public:
    Shape(const std::string&);
    virtual ~Shape() = default; // compiler generates virtual destructor

    void setName(const std::string&);
    std::string getName() const;

    virtual double getArea() const = 0; // get the surface area          
    virtual double getVolume() const = 0; // get the volume          
    virtual bool test(const std::vector<std::string>& cond) const = 0; // test if the conditions are statisfied

    virtual std::string getInfo() const = 0; // get all the info in a string

private:
    std::string name;

};