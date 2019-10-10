const fs = require('fs');
const path = require('path');

function uniqueSharedPtrs(config) {
  const re = /std::shared_ptr<([^>]+)>/g;
  const stringifiedConfig = JSON.stringify(config);
  let m;
  const values = [];
  do {
    m = re.exec(stringifiedConfig);
    if (m)
      values.push(m[1]);
  } while (m);
    return Array.from(new Set(values));
}

// functions exposed to templates
const gen = {
  capitalize: value => value.charAt(0).toUpperCase() + value.slice(1),
  str: value => `V8PerIsolateData::String(isolate, "${value}")`
};

const debugInvocations = true;

const v8ClassNameCppTemplate = fs.readFileSync(path.join(__dirname, 'V8ClassName.cpp.template'), 'utf8');
const v8ClassNameHTemplate = fs.readFileSync(path.join(__dirname, 'V8ClassName.h.template'), 'utf8');
// const v8ClassNameTypesCppTemplate = fs.readFileSync(path.join(__dirname, 'V8ClassNameTypes.cpp.template'), 'utf8');
// const v8ClassNameTypesHTemplate = fs.readFileSync(path.join(__dirname, 'V8ClassNameTypes.h.template'), 'utf8');

JSON.parse(fs.readFileSync(process.argv[2], 'utf8')).forEach(config => {
  config.typesDependencies = config.typesDependencies || []
  config.typesDependencies.push(...uniqueSharedPtrs(config.types));

  config.implDependencies = config.implDependencies || []
  config.implDependencies.push(...uniqueSharedPtrs([config.constants, config.properties, config.functions]));

  // if (config.types) {
  //   fs.writeFileSync('V8' + config.className + 'Types.h', eval('`' + v8ClassNameTypesHTemplate + '`'));
  //   fs.writeFileSync('V8' + config.className + 'Types.cpp', eval('`' + v8ClassNameTypesCppTemplate + '`'));
  // }
  fs.writeFileSync('V8' + config.className + '.h', eval('`' + v8ClassNameHTemplate + '`'));
  fs.writeFileSync('V8' + config.className + '.cpp', eval('`' + v8ClassNameCppTemplate + '`'));
});
