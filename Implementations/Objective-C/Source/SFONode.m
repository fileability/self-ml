#import "SFONode.h"

#ifndef SFO_UTF8_TO_NSSTRING
#define SFO_UTF8_TO_NSSTRING(utf) [[[NSString alloc] initWithUTF8String:utf] autorelease]
#endif

@interface SFONode ()

- (void)cleanUp;

@end


@implementation SFONode

@synthesize children;
@synthesize parent;
@synthesize rootNode;

#pragma mark Creation

+ (id)node
{
	return [[[self alloc] init] autorelease];
}
+ (id)nodeFromString:(NSString *)string
{
	return [[[self alloc] initWithString:string] autorelease];
}
+ (id)nodeFromList:(NSArray *)strings
{
	return [[[self alloc] initWithList:strings] autorelease];
}
+ (id)nodeFromData:(NSData *)data
{
	return [[[self alloc] initWithData:data] autorelease];
}
+ (id)nodeWithContentsOfFile:(NSString *)path
{
	return [[[self alloc] initWithContentsOfFile:path] autorelease];
}
+ (id)nodeFromNodeRef:(SFNodeRef)ref
{
	return [[[self alloc] initWithNodeRef:ref] autorelease];
}

- (id)init
{
	SFNodeRef ref = SFNodeCreate();
	return [self initWithNodeRef:ref];
}
- (id)initWithString:(NSString *)string
{
	SFNodeRef ref = SFNodeCreateFromString([string UTF8String]);
	return [self initWithNodeRef:ref];
}
- (id)initWithList:(NSArray *)strings
{
	if (self = [self init])
	{
		NSUInteger i = 0;
		for (i = 0; i < [strings count]; i++)
		{
			if (i == 0)
				self.head = [strings objectAtIndex:i];
			else
				[self addChild:[strings objectAtIndex:i]];
		}
	}
	return self;
}


- (id)initWithData:(NSData *)data
{
	SFNodeRef ref = SFNodeCreateFromString([[[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding] autorelease] UTF8String]);
	return [self initWithNodeRef:ref];
}
- (id)initWithContentsOfFile:(NSString *)path
{
	NSError *err = nil;
	NSString *str = [[NSString alloc] initWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
	
	if (!str || err)
		return nil;
	
	return [self initWithString:str];
}

//Designated Initializer
- (id)initWithNodeRef:(SFNodeRef)ref
{
	if (self = [super init])
	{
		if (SFNodeGetType(ref) == SFNodeTypeString)
		{
			if (!SFNodeStringValue(ref))
				return @"";
			return [NSString stringWithUTF8String:SFNodeStringValue(ref)];
		}
		
		node = ref;
		
		rootNode = self;
		
		//Check that the backing node isn't null
		if (node == SFNullNode)
			return nil;
		
		
		//Count the number of children
		NSUInteger childCount = 0;
		SFNodeRef currentChild = SFNodeFirstChild(node);
		while (currentChild != SFNullNode)
		{
			childCount++;
			currentChild = SFNodeNextInList(currentChild);
		}
		
		
		//Add the children, along with new nodes
		children = [[NSMutableArray alloc] initWithCapacity:childCount];
		currentChild = SFNodeFirstChild(node);
		while (currentChild != SFNullNode)
		{
			id newNode = [[self class] nodeFromNodeRef:currentChild];
			[children addObject:newNode];
			currentChild = SFNodeNextInList(currentChild);
		}
	}
	
	return self;
}


- (id)copyWithZone:(NSZone *)zone
{
	return [[[[self class] allocWithZone:zone] initWithNodeRef:SFNodeCopy(node)] autorelease];	
}


#pragma mark Equality, etc

- (BOOL)isEqual:(id<SFONodeChild>)otherNode
{
	return [[self selfmlRepresentation] isEqual:[otherNode selfmlRepresentation]];
}


#pragma mark Properties and Getters

- (SFNodeRef)nodeRef
{
	return node;
}

- (NSString *)head
{
	const char* head = SFNodeHead(node);
	if (!head)
		return @"";
	
	return [[[NSString alloc] initWithUTF8String:head] autorelease] ?: @"";
}
- (void)setHead:(NSString *)headString
{
	size_t len = strlen([headString UTF8String]);
	char* head = malloc((len + 1) * sizeof(char));
	strlcpy(head, [headString UTF8String], (len + 1));
	
	if (!head || strlen(head) == 0)
		return;
	
	SFNodeSetHead(node, head);
}

- (NSUInteger)childCount
{
	return [children count];
}

- (id<SFONodeChild>)childAtIndex:(NSUInteger)index
{
	return [children objectAtIndex:index];
}

- (NSUInteger)indexOfChildNode:(id<SFONodeChild>)childNode
{
	return [[self children] indexOfObject:childNode];
}

- (void)replaceChildNodeAtIndex:(NSInteger)index with:(id<SFONodeChild>)newChild
{
	if (index < 0)
		return;
	
	NSUInteger previousCount = [self childCount];
	
	//Add a child node to the end
	[self addChild:newChild];
	
	if (index >= previousCount)
		return;
	
	SFONode *b = [children objectAtIndex:index];
	if ([b respondsToSelector:@selector(setNodeRef:)])
		[b setNodeRef:SFNullNode];
	
	SFNodeReplaceChildAtIndexWithLast(node, index);
	
	if ([self childCount] <= 1)
	{
		return;
	}
	
	if (newChild != nil)
	{
		[children replaceObjectAtIndex:index withObject:[children lastObject]];
		[children removeLastObject];
	}
}

//TODO: IMPLEMENT OTHER PROPERTIES


#pragma mark Tree Manipulation

- (void)addChild:(id<SFONodeChild>)newNode
{	
	id item = newNode;
	
	if ([item sfNodeType] == SFNodeTypeString)
	{
		item = [[item copy] autorelease];
		
		//Get the UTF8 value of item, then create a new child node and append it to node
		size_t len = strlen([(NSString *)item UTF8String]);
		char* str = malloc((len + 1) * sizeof(char));
		strlcpy(str, [(NSString *)item UTF8String], (len + 1));
		
		SFNodeAddString(node, str);
	}
	else if ([item sfNodeType] == SFNodeTypeList)
	{
		//Add item as a child
		SFNodeAddChild(node, [item nodeRef]);
		
		//Remember to set the parent and root node!
		[(SFONode *)item setParent:self];
		[(SFONode *)item setRootNode:[self rootNode]];
	}
	[children addObject:item];
}

/*
- (void)insertChild:(id<SFONodeChild>)childNode atIndex:(NSUInteger)index
{
	//TODO: IMPLEMENT ME
}
- (void)replaceChildAtIndex:(NSUInteger)index withNode:(id<SFONodeChild>)childNode
{
	//TODO: IMPLEMENT ME
}
- (void)removeChildAtIndex:(NSUInteger)index
{
	//TODO: IMPLEMENT ME
}
*/


- (SFNodeType)sfNodeType
{
	return SFNodeTypeList;
}


#pragma mark Querying

//Extract an NSArray of all child nodes with name nodeName
- (NSArray *)extract:(NSString *)nodeName
{
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if ([child sfNodeType] == SFNodeTypeList && [[child head] isEqual:nodeName]) {
			[result addObject:child];
		}
	}
	return result;
}

//Extract all strings
- (NSArray *)extractStrings
{
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if([child sfNodeType] == SFNodeTypeString) {
			[result addObject:child];
		}
	}
	
	return result;
}

//Extract singleton nodes (like) (this)
- (NSArray *)extractSingletonNodes
{
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if([child sfNodeType] == SFNodeTypeList && [[child head] length] > 0 && [child childCount] == 0) {
			[result addObject:child];
		}
	}
	
	return result;
}

- (NSArray *)extractLists
{
	NSMutableArray *result = [[[NSMutableArray alloc] init] autorelease];
	for(SFONode *child in children) {
		if([child sfNodeType] == SFNodeTypeList) {
			[result addObject:child];
		}
	}
	
	return result;
}

- (id)firstIfString
{
	id first = [self first];
	if ([first sfNodeType] == SFNodeTypeString)
		return first;
	return nil;
}
- (id)first
{
	return [[self children] firstObject];
}
- (NSArray *)rest
{
	if ([children count] >= 2)
		return [children subarrayWithRange:NSMakeRange(1, [children count] - 1)];
	return nil;
}
- (id)nodeForKey:(NSString *)key
{
	return [[self extract:key] firstObject];
}
- (id)valueForKey:(NSString *)key
{
	SFONode *forKey = [self nodeForKey:key];
	
	if (!forKey)
		return [super valueForKey:key];
	
	NSString *firstIfString = [forKey firstIfString];
	if ([forKey childCount] == 1 && firstIfString)
		return firstIfString;
	
	return forKey;
}
- (void)setValue:(id)value forKey:(NSString *)key
{
	SFONode *forKey = [self nodeForKey:key];
	
	if (!forKey)
		return [super setValue:value forKey:key];
	
	NSString *firstIfString = [forKey firstIfString];
	if ([forKey childCount] == 1 && firstIfString)
	{
		[forKey replaceChildNodeAtIndex:0 with:value];
	}
	else
	{
		NSInteger index = [self indexOfChildNode:forKey];
		if (index < 0 || index >= NSNotFound)
			return;
		
		[self replaceChildNodeAtIndex:index with:value];
	}
}

- (id)valueForUndefinedKey:(NSString *)key
{
	return nil;
}

- (BOOL)hasSingletonNodeWithHead:(NSString *)shead
{
	for (SFONode *child in children)
	{
		if ([child sfNodeType] == SFNodeTypeList && [child childCount] == 0)
		{
			if ([[child head] isEqual:shead])
				return YES;
		}
	}
	
	return NO;
}


#pragma mark Output

//Use NSFileHandle -> fileDescriptor -> fdopen to create a FILE* to feed to SFNodeWriteRepresentationToFile()
- (NSString *)selfmlRepresentation
{
	NSMutableString *stringRep = [[[NSMutableString alloc] init] autorelease];
	SFONodeWriteRepresentation([self nodeRef], stringRep);
	return stringRep;
	
}

//Use NSXMLDocument to create an XML string
- (NSString *)xmlRepresentation
{
	//TODO: IMPLEMENT ME
	return nil;
}


#pragma mark Cleanup

/*
- (void)dealloc
{
	[self cleanUp];
	
	NSLog(@" -");
	NSLog(@"children = %d", [children count]);
	[children release];
	
	[super dealloc];
}
*/
- (void)finalize
{
	[self cleanUp];
	[super finalize];
}
- (void)cleanUp
{	
	if (node != SFNullNode)
	{
		SFNodeFreeNonRecursive(node);
		node = SFNullNode;
	}
}

#pragma mark Functions
void SFONodeWriteRepresentation(SFNodeRef node, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
    
    SFONodeWriteRepresentationInner(node, 0, mstr);
}
void SFONodeWriteRepresentationInner(SFNodeRef node, int indentation, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
    
    int i;
    for (i = 0; i < indentation; i++)
    {
        [mstr appendFormat:@"    "];
    }
    
    if (SFNodeGetType(node) == SFNodeTypeList)
    {
        SFONodeWriteRepresentationOfList(node, indentation, mstr);
    }
    else if (SFNodeGetType(node) == SFNodeTypeString)
    {  
        SFONodeWriteRepresentationOfString(node, mstr);
    }
}

void SFONodeWriteRepresentationOfList(SFNodeRef node, int indentation, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
	
    const char *head = SFNodeHead(node);
    _Bool isRoot = head == NULL;
	
    if (!isRoot)
        [mstr appendFormat:@"(%@", SFO_UTF8_TO_NSSTRING(SFNodeHead(node))];
	
    SFNodeRef r = SFNodeFirstChild(node);
    _Bool isScalarOnly = true;
    if (isRoot)
    {
        isScalarOnly = false;
    }
    else
    {
        while (r != SFNullNode)
        {
            if (SFNodeGetType(r) == SFNodeTypeList)
                isScalarOnly = false;
			
            r = SFNodeNextInList(r);
        }
    }
	
    r = SFNodeFirstChild(node);
    _Bool isFirstChild = true;
    while (r != SFNullNode)
    {
		_Bool isScalar = SFNodeGetType(r) == SFNodeTypeString;
		_Bool isSingleton = SFNodeGetType(r) == SFNodeTypeList && SFNodeFirstChild(r) == SFNullNode;
		
		if (isRoot)
        {
            if (!isFirstChild)
                [mstr appendFormat:@"\n\n"];
            
            SFONodeWriteRepresentationInner(r, 0, mstr);
        }
		else if (isFirstChild && (isScalar || isSingleton))
		{
			[mstr appendFormat:@" "];
            SFONodeWriteRepresentationInner(r, 0, mstr);
		}
        else if (isScalarOnly)
        {
            [mstr appendFormat:@" "];
            SFONodeWriteRepresentationInner(r, 0, mstr);
        }
        else
        {
            [mstr appendFormat:@"\n"];
            SFONodeWriteRepresentationInner(r, indentation + 1, mstr);
        }
		
        r = SFNodeNextInList(r);
        isFirstChild = false;
    }
	
    if (!isRoot)
        [mstr appendFormat:@")"];
}
void SFONodeWriteRepresentationOfString(SFNodeRef node, NSMutableString *mstr)
{
    if (node == SFNullNode)
        return;
    
    const char *strval = SFNodeStringValue(node);
    if (strval == NULL)
        return;
	
    //Find out if scannerStrval can be written as a verbatim string or bracketed string
    _Bool isVerbatimString = true;
    _Bool isBracketedString = true;
    
    int bracketedStringNestingLevel = 0;
    const char *scannerStrval = strval;
    for (; *scannerStrval != '\0'; scannerStrval++)
    {
        if (isspace(*scannerStrval))
        {
            isVerbatimString = false;
        }
        
        if (*scannerStrval == '[')
            bracketedStringNestingLevel++;
        else if (*scannerStrval == ']')
            bracketedStringNestingLevel--;
        
        if (bracketedStringNestingLevel == -1)
            isBracketedString = false;
        
        switch (*scannerStrval) {
            case '#':
            case '`':
            case '(':
            case ')':
            case '[':
            case ']':
            case '{':
            case '}':
                isVerbatimString = false;
            default: continue;
        }
    }
    
    
    if (isVerbatimString)
    {
        [mstr appendString:SFO_UTF8_TO_NSSTRING(strval)];
    }
    else if (isBracketedString && bracketedStringNestingLevel == 0)
    {
        [mstr appendFormat:@"[%@]", SFO_UTF8_TO_NSSTRING(strval)];
    }
    else
    {
        [mstr appendFormat:@"`"];
        for (; *strval != '\0'; strval++)
        {
            if (*strval == '`')
                [mstr appendFormat:@"`"];
			
            [mstr appendFormat:@"%c", *strval];
        }
        [mstr appendFormat:@"`"];
    }
}


@end