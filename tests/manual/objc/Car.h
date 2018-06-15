// Car.h
#import <Foundation/Foundation.h>

@interface Car : NSObject {
    // Protected instance variables (not recommended)
}

@property (copy) NSString *model;

- (void)drive;
- (id)initWithModel:(NSString *)aModel;
+ (void)setDefaultModel:(NSString *)aModel;

@end
