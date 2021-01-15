package water.clustering.api;

import fi.iki.elonen.NanoHTTPD;
import fi.iki.elonen.router.RouterNanoHTTPD;
import water.H2O;
import water.H2ONode;

import java.util.Map;

public class H2OClusterStatusEndpoint extends RouterNanoHTTPD.DefaultHandler {

    @Override
    public String getText() {
        throw new IllegalStateException(String.format("Method getText should not be called on '%s'",
                getClass().getName()));
    }

    @Override
    public String getMimeType() {
        return "application/json";
    }

    @Override
    public NanoHTTPD.Response.IStatus getStatus() {
        throw new IllegalStateException(String.format("Method getMimeType should not be called on '%s'",
                getClass().getName()));
    }

    @Override
    public NanoHTTPD.Response get(RouterNanoHTTPD.UriResource uriResource, Map<String, String> urlParams, NanoHTTPD.IHTTPSession session) {
        // All nodes report ready state until the clustering process is finished. Since then, only the leader node is ready.
        final boolean isClustered = H2O.CLOUD._memary.length == 0;
        if (isClustered) {
            return NanoHTTPD.newFixedLengthResponse(NanoHTTPD.Response.Status.NO_CONTENT, getMimeType(), null);
        } else {
            return NanoHTTPD.newFixedLengthResponse(NanoHTTPD.Response.Status.OK, getMimeType(), nodesListJson());
        }
    }

    private String nodesListJson() {
        final H2ONode[] cloudMembers = H2O.CLOUD.members();

        final StringBuilder healthyNodesStringArray = new StringBuilder();
        final StringBuilder unhealthyNodesStringArray = new StringBuilder();
        int healthyNodeCount = 0;
        int unhealthyNodeCount = 0;
        for (int i = 0; i < cloudMembers.length; i++) {
            final H2ONode node = cloudMembers[i];
            if (node.isHealthy()) {
                healthyNodesStringArray.append('"');
                healthyNodesStringArray.append(node.getIpPortString());
                healthyNodesStringArray.append("\",");
                healthyNodeCount++;
            } else {
                unhealthyNodesStringArray.append('"');
                unhealthyNodesStringArray.append(node.getIpPortString());
                unhealthyNodesStringArray.append('"');
                unhealthyNodesStringArray.append("\",");
                unhealthyNodeCount++;
            }
        }
        if (healthyNodeCount > 0) {
            healthyNodesStringArray.deleteCharAt(healthyNodesStringArray.length() - 1);
        }

        if (unhealthyNodeCount > 0) {
            unhealthyNodesStringArray.deleteCharAt(unhealthyNodesStringArray.length() - 1);
        }

        return String.format("{\n" +
                        "\"healthy_nodes\": [%s],\n" +
                        "\"unhealthy_nodes\": [%s]\n" +
                        "}", healthyNodesStringArray.toString(),
                unhealthyNodesStringArray.toString());
    }
}
