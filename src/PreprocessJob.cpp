/* This file is part of RTags.

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "PreprocessJob.h"
#include "RTagsClang.h"
#include "Server.h"
#include "Cpp.h"

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
            std::shared_ptr<Cpp> cpp = RTags::preprocess(source);
            if (!cpp) {
                error() << "Couldn't preprocess" << source.sourceFile();
                return;
            }

            if (mAsync) {
                List<String> projects = std::move(mProjects);
                EventLoop::mainEventLoop()->callLater([projects, cpp, unresolvedPath, source]() {
                        Server::instance()->index(std::move(source), std::move(projects),
                                                  std::move(unresolvedPath), std::move(cpp->preprocessed));
                    });
            } else {
                Server::instance()->index(std::move(source), std::move(mProjects),
                                          std::move(unresolvedPath), std::move(cpp->preprocessed));
            }
            break; }
        default:
            break;
        }
    }
}
