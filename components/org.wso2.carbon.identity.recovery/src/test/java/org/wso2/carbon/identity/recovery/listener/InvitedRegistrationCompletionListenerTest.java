/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.listener;

import org.mockito.Mockito;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;

import static org.testng.Assert.assertTrue;

public class InvitedRegistrationCompletionListenerTest {

//    private InvitedRegistrationCompletionListener listener;
//    private FlowExecutionContext context;
//    private FlowExecutionStep step;
//
//    @BeforeMethod
//    public void setUp() {
//        listener = new InvitedRegistrationCompletionListener();
//        context = Mockito.mock(FlowExecutionContext.class);
//        step = Mockito.mock(FlowExecutionStep.class);
//    }
//
//    @Test
//    public void testDoPreExecute() throws Exception {
//        // Simulate necessary properties for pre execution.
//        Mockito.when(context.getProperty("confirmationCode")).thenReturn("sampleCode");
//        // Stub additional properties as needed in your actual logic.
//        assertTrue(listener.doPreExecute(context));
//    }
//
//    @Test
//    public void testDoPostExecuteWhenFlowComplete() throws Exception {
//        // Set up context for a complete flow execution.
//        Mockito.when(step.getFlowStatus()).thenReturn("COMPLETE");
//        Mockito.when(context.getFlowType()).thenReturn(IdentityRecoveryConstants.ASK_PASSWORD_FLOW_TYPE);
//        Mockito.when(context.getProperty("confirmationCode")).thenReturn("sampleCode");
//
//        // Provide a dummy UserRecoveryData object.
//        UserRecoveryData recoveryData = new UserRecoveryData();
//        Mockito.when(context.getProperty(IdentityRecoveryConstants.USER_RECOVERY_DATA))
//                .thenReturn(recoveryData);
//
//        // Expect that the listener executes the post logic without exception.
//        assertTrue(listener.doPostExecute(step, context));
//    }
//
//    @Test
//    public void testDoPostExecuteWhenFlowNotComplete() throws Exception {
//        // When flow status is not complete, listener should simply return true.
//        Mockito.when(step.getFlowStatus()).thenReturn("INCOMPLETE");
//        assertTrue(listener.doPostExecute(step, context));
//    }
}