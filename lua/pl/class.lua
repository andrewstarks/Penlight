--- Provides a reuseable and convenient framework for creating classes in Lua.
-- Two possible notations:
--
--    B = class(A)
--    class.B(A)
--
-- The latter form creates a named class within the current environment. Note
-- that this implicitly brings in `pl.utils` as a dependency.
--
-- See the Guide for further @{01-introduction.md.Simplifying_Object_Oriented_Programming_in_Lua|discussion}
-- @module pl.class

local error, getmetatable, io, pairs, rawget, rawset, setmetatable, tostring, type =
    _G.error, _G.getmetatable, _G.io, _G.pairs, _G.rawget, _G.rawset, _G.setmetatable, _G.tostring, _G.type
local compat

-- this trickery is necessary to prevent the inheritance of 'super' and
-- the resulting recursive call problems.
local function call_ctor (c,obj,...)
    -- nice alias for the base class ctor
    local base = rawget(c,'_base')
    if base then
        local parent_ctor = rawget(base,'_init')
        while not parent_ctor do
            base = rawget(base,'_base')
            if not base then break end
            parent_ctor = rawget(base,'_init')
        end
        if parent_ctor then
            rawset(obj,'super',function(obj,...)
                call_ctor(base,obj,...)
            end)
        end
    end
    local res = c._init(obj,...)
    rawset(obj,'super',nil)
    return res
end

--- initializes an __instance__ upon creation.
-- @function class:_init
-- @param ... parameters passed to the constructor
-- @usage local Cat = class()
-- function Cat:_init(name)
--   --self:super(name)   -- call the ancestor initializer if needed
--   self.name = name
-- end
--
-- local pussycat = Cat("pussycat")
-- print(pussycat.name)  --> pussycat

--- checks whether an __instance__ is derived from some class.
-- Works the other way around as `class_of`.
-- @function instance:is_a
-- @param some_class class to check against
-- @return `true` if `instance` is derived from `some_class`
-- @usage local pussycat = Lion()  -- assuming Lion derives from Cat
-- if pussycat:is_a(Cat) then
--   -- it's true
-- end
local function is_a(self,klass)
    local m = getmetatable(self)
    if not m then return false end --*can't be an object!
    while m do
        if m == klass then return true end
        m = rawget(m,'_base')
    end
    return false
end

--- checks whether an __instance__ is derived from some class.
-- Works the other way around as `is_a`.
-- @function some_class:class_of
-- @param some_instance instance to check against
-- @return `true` if `some_instance` is derived from `some_class`
-- @usage local pussycat = Lion()  -- assuming Lion derives from Cat
-- if Cat:class_of(pussycat) then
--   -- it's true
-- end
local function class_of(klass,obj)
    if type(klass) ~= 'table' or not rawget(klass,'is_a') then return false end
    return klass.is_a(obj,klass)
end

--- cast an object to another class.
-- It is not clever (or safe!) so use carefully.
-- @param some_instance the object to be changed
-- @function some_class:cast
local function cast (klass, obj)
    return setmetatable(obj,klass)
end


local function _class_tostring (obj)
    local mt = obj._class
    local name = rawget(mt,'_name')
    setmetatable(obj,nil)
    local str = tostring(obj)
    setmetatable(obj,mt)
    if name then str = name ..str:gsub('table','') end
    return str
end

local function tupdate(td,ts,dont_override)
    for k,v in pairs(ts) do
        if not dont_override or td[k] == nil then
            td[k] = v
        end
    end
end

--[[ TODO: There needs to be more documentation about:
	* _class_init: something that a base class does to the clases that inheret from it?
	* _post_init: a base class method that is run post-_init of the inhereted class.
	* _create: a function that is run *on the arguments received*. I believe this is
		intended to be a sort of default argument processor?
		intended to be a sort of default argument processor?
--]]
local function _class(base,c_arg,c)
    -- the class `c` will be the metatable for all its objects,
    -- and they will look up their methods in it.
    local mt = {}   -- a metatable for the class to support __call and _handler
    -- can define class by passing it a plain table of methods
    local plain = type(base) == 'table' and not getmetatable(base)
    if plain then
        c = base
        base = c._base
    else
        c = c or {}
    end
   
    if type(base) == 'table' then
        -- our new class is a shallow copy of the base class!
        -- but be careful not to wipe out any methods we have been given at this point!
        tupdate(c,base,plain)
        c._base = base
        -- inherit the 'not found' handler, if present
        if rawget(c,'_handler') then mt.__index = c._handler end
    elseif base ~= nil then
        error("must derive from a table type",3)
    end

    c.__index = c
    setmetatable(c,mt)
    if not plain then
        c._init = nil
    end

    if base and rawget(base,'_class_init') then
        base._class_init(c,c_arg)
    end

    -- expose a ctor which can be called by <classname>(<args>)
    mt.__call = function(class_tbl,...)
        local obj
        if rawget(c,'_create') then obj = c._create(...) end
        if not obj then obj = {} end
        setmetatable(obj,c)

        if rawget(c,'_init') then -- explicit constructor
            local res = call_ctor(c,obj,...)
            if res then -- _if_ a ctor returns a value, it becomes the object...
                obj = res
                setmetatable(obj,c)
            end
        elseif base and rawget(base,'_init') then -- default constructor
            -- make sure that any stuff from the base class is initialized!
            call_ctor(base,obj,...)
        end

        if base and rawget(base,'_post_init') then
            base._post_init(obj)
        end

        if not rawget(c,'__tostring') then
            c.__tostring = _class_tostring
        end
        return obj
    end
    -- Call Class.catch to set a handler for methods/properties not found in the class!
    c.catch = function(self, handler)
        if type(self) == "function" then
            -- called using . instead of :
            handler = self
        end
        c._handler = handler
        mt.__index = handler
    end
    c.is_a = is_a
    c.class_of = class_of
    c.cast = cast
    c._class = c

    return c
end

--- create a new class, derived from a given base class.
-- Supporting two class creation syntaxes:
-- either `Name = class(base)` or `class.Name(base)`.
-- The first form returns the class directly and does not set its `_name`.
-- The second form creates a variable `Name` in the current environment set
-- to the class, and also sets `_name`.
-- if `class.use_globals` is set to `true`, then classes are stored in _ENV (or global, for Lua < 5.2). If `class.use_globals` is `false` or `nil`, then classes are stored within the `class.classes` field. Unlike the `true` variant, subsequent indexing of the class name results in the existing class being returned, instead of the creation of another new class by the same name. Deleting a class becomes `class.class_name = nil`.
-- @function class
-- @param base optional base class
-- @param c_arg optional parameter to class constructor
-- @param c optional table to be used as class
local class
--for strict, so it doesn't complain later.
local _PL_GLOBAL_CLASSES = _PL_GLOBAL_CLASSES == nil and true or _PL_GLOBAL_CLASSES
class = setmetatable({classes = {}, use_globals = _PL_GLOBAL_CLASSES},{
    __call = function(fun,...)
        return _class(...)
    end,
    __index = function(tbl,key)
        if key == 'class' then
            io.stderr:write('require("pl.class").class is deprecated. Use require("pl.class")\n')
            return class
        end
		local env
		if rawget(tbl, 'use_globals') then
			compat = compat or require 'pl.compat'
			env = compat.getfenv(2)
		elseif tbl.classes[key] then --access existing class
			return tbl.classes[key]
		else --create new class, without global namespace pollution.
			env = tbl.classes
		end
		-- return a closure that will set the key of the 
		-- global / class.classes table to the new class that is created.
		return function(...)
            local c = _class(...)
            c._name = key
            rawset(env,key,c)
            return c
        end
    end,
	__newindex = function(tbl,key,value)
		if tbl.classes[key] then
			tbl.classes[key] = value
		else
			rawset(tbl, key, value)
		end
	end,
})

class.properties = class()

function class.properties._class_init(klass)
    klass.__index = function(t,key)
        -- normal class lookup!
		-- needs to be raw because of `catch` and _handler
        local value = rawget(klass,key)
        if value then return value end
        -- is it a getter?
		if type(key) == 'string' then
			value = rawget(klass,'get_'..key)
			if value then
				return value(t)
			end
			-- is it a field?
			value = rawget(t,'_'..key)
			if value ~= nil then
				return value
			end
		end
		if klass._handler then
			return klass._handler(t, key)
		else
			 return nil
		end

    end
    klass.__newindex = function (t,key,value)
        -- if there's a setter, use that, otherwise directly set table
		local setter
		if  type(key) == 'string' then
			-- must be raw because of _setter
			setter = rawget(klass, 'set_'..key)
			if setter then
				--in this case, we have an explicit setter.
				setter(t,value)
			elseif setter == false then
				--setter == false, which means read-only.
				--It's debatable, whether or not to do this. I think that it's a good idea, given that an error would
				--make it much easier to debug.
				error(("The '%s' field is read-only in the '%s' class."):
					format( tostring(key), 
						tostring(klass._name or t._name or ("<unknown class name: %s>"):format(tostring(t)) ) 
					),
				2)
			end
		end
		--now check for the implicit setter, if there is one.
		--compliments _handler
		setter = rawget(klass, '_setter')
		if setter and setter ~= klass.__newindex then
			return klass._setter(t, key, value)
		else --all else failed...
			rawset(t,key,value)
		end
    end
end


return class

