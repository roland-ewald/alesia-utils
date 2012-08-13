/*
 * Copyright 2012 Roland Ewald
 *  
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at 
 *  
 *  http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and 
 * limitations under the License. 
 */
package alesia.tools

import org.junit.runner.RunWith
import alesia.tools.output.TestResultAggregator
import org.junit.runners.Suite.SuiteClasses
import alesia.tools.math.TestShannonEntropy
import alesia.tools.math.TestJensenShannonDivergence
import alesia.tools.bdd.TestBDDNode
import alesia.tools.bdd.TestBDDProcessing

/** Bundles all tests together.
 *  @author Roland Ewald
 */
@RunWith(value = classOf[org.junit.runners.Suite])
@SuiteClasses(value = Array(classOf[TestBDDProcessing], classOf[TestBDDNode], classOf[TestResultAggregator], classOf[TestShannonEntropy], classOf[TestJensenShannonDivergence]))
class AllTests