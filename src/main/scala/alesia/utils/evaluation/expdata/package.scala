package alesia.utils.evaluation

import sessl.AbstractPerformanceObservation
import sessl.AbstractExperiment

/**
 * Some auxiliary definitions.
 *
 * @author Roland Ewald
 */
package object expdata {

  type RuntimeExperiment = AbstractExperiment with AbstractPerformanceObservation

}