#include "PreprocessJob.h"
#include "RTagsClang.h"
#include "Server.h"

PreprocessJob::PreprocessJob(String &&arguments, Path &&workingDirectory, List<String> &&projects)
    : mAsync(true),
      mArguments(std::forward<String>(arguments)),
      mWorkingDirectory(std::forward<Path>(workingDirectory)),
      mProjects(std::forward<List<String> >(projects))
{
}

void PreprocessJob::run()
{
    assert(mWorkingDirectory.endsWith('/'));
    Path unresolvedPath;
    Source source = Source::parse(mArguments, mWorkingDirectory, &unresolvedPath);
    if (!source.isNull()) {
        switch (source.language) {
        case Source::C:
        case Source::CPlusPlus:
        case Source::CPlusPlus11: {
            String preprocessed = RTags::preprocess(source);
            if (mAsync) {
                List<String> projects = std::move(mProjects);
                EventLoop::mainEventLoop()->callLater([projects, preprocessed, unresolvedPath, source]() {
                        Server::instance()->index(std::move(source), std::move(projects),
                                                  std::move(unresolvedPath), std::move(preprocessed));
                    });
            } else {
                Server::instance()->index(std::move(source), std::move(mProjects),
                                          std::move(unresolvedPath), std::move(preprocessed));
            }
            break; }
        default:
            break;
        }
    }
}
